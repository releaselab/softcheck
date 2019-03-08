module Make_common(Expr : sig
    type expr
    val expr_to_string : expr -> string
  end) = struct
  module V = struct
    type t = Expr.expr Cfg_node.t
    let compare x y = compare x.Cfg_node.id y.Cfg_node.id
    let hash x = Hashtbl.hash x.Cfg_node.id
    let equal x y = x.Cfg_node.id = y.Cfg_node.id
  end

  module E = struct
    type t = Normal | If_true | If_false
    let compare = compare
    let default = Normal

    let to_string = function
        Normal  -> ""
      | If_true   -> "true"
      | If_false    -> "false"
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(V)(E)

  type vertex = V.t
  type edge_label = E.t

  module Display(X : sig
      val label_to_subgraph : int -> Graph.Graphviz.DotAttributes.subgraph
      val label_to_dot_label : int -> string end) = struct
    include G

    let vertex_name v = string_of_int v.Cfg_node.id
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape(`Box); `Fontname("Courier")]
    let vertex_attributes v = [`Label(X.label_to_dot_label v.Cfg_node.id)]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph v = Some(X.label_to_subgraph v.Cfg_node.id)
  end

  module Wrapper = struct
    let inflow = G.pred

    let outflow = G.succ

    let is_extremal exts l = List.mem l exts

    let get = Hashtbl.find

    let add b g p func_id v =
      let open Cfg_node in
      Hashtbl.replace b v.id v;
      Hashtbl.replace p v.id func_id; G.add_vertex g v

    let connect g label l l' = G.add_edge_e g (G.E.create l label l')

    let dot_output b g p f =
      let module Helper =
      struct
        let label_to_dot_label l =
          let n = Hashtbl.find b l in
          Printf.sprintf "[%s]^%d" (Cfg_node.to_string Expr.expr_to_string (n.Cfg_node.stmt)) l
        let label_to_subgraph l =
          let fid = Hashtbl.find p l in
          {Graph.Graphviz.DotAttributes.sg_name=fid; sg_attributes=[`Label fid]; sg_parent=None}
      end
      in
      let module Dot_ = Graph.Graphviz.Dot(Display(Helper)) in
      let oc = open_out f in
      Dot_.output_graph oc g;
      close_out oc

    let display_with_gv b g p =
      let tmp_dot = Filename.temp_file "graph" ".dot" in
      dot_output b g p tmp_dot;
      let tmp_ps = Filename.temp_file "graph" ".ps" in
      ignore(Sys.command ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^ tmp_ps ^ " &"));
      Sys.remove tmp_dot
  end
end

module Make_cfg(Expr : sig
    type expr
    val expr_to_string : expr -> string
  end)
    (C : sig
       type program
       type decl
       type expr
       type stmt

       val stmt_to_node : stmt -> expr Softlang.t
       val global_decls : program -> decl list
       val funcs : program -> (string * string list * expr Softlang.t) list
     end with type expr = Expr.expr) = struct
  include Make_common(Expr)

  type t = {
    blocks: (int, V.t) Hashtbl.t;
    flow: G.t;
    functions: (int, string) Hashtbl.t;
    mutable extremals: int list;
    mutable extremalsR: int list
  }

  let create () = {
    blocks = Hashtbl.create 10; flow= G.create();
    functions= Hashtbl.create 10; extremals= []; extremalsR = []
  }
  let inflow { flow=g; _ } = Wrapper.inflow g
  let outflow { flow=g; _ } = Wrapper.outflow g
  let is_extremal t = Wrapper.is_extremal t.extremals
  let is_extremalR t = Wrapper.is_extremal t.extremalsR
  let get t = Wrapper.get t.blocks
  let add { blocks=b; flow=g; functions=p; _ } = Wrapper.add b g p
  let connect { flow=g; _ } ?(label = E.default) = Wrapper.connect g label
  let get_blocks { blocks=b; _ } = b
  let get_func_id { functions=p; _ } = Hashtbl.find p
  let extremal t l = t.extremals <- l::t.extremals
  let extremalR t l = t.extremalsR <- l::t.extremalsR

  let dot_output { blocks=b; flow=g; functions=p; _ } = Wrapper.dot_output b g p

  let display_with_gv { blocks=b; flow=g; functions=p; _ } =
    Wrapper.display_with_gv b g p

  let show = display_with_gv

  let generate_from_program (p : C.program) =
    let open Batteries in
    let graph = create() in (* graph -> Make2.t *)
    let pBlocks = Hashtbl.create 10 in (* pBlocks -> Hashtbl.t *)
    let funcs = C.funcs p in
    let () = List.map (fun (_, _, b) -> Flow.init b) funcs |>
             List.iter (extremal graph) in
    let () = List.map (fun (_, _, b) -> Flow.final b) funcs |>
             List.iter (Set.iter (extremalR graph)) in
    let () =
      Hashtbl.iter (fun id blocks -> Set.iter (add graph id) blocks) pBlocks in
    let add_edge (i, j) = connect graph i j in
    let () = List.iter (fun (f, _, b) ->
      let nodes, flow = Flow.flow b in
      let () = Hashtbl.replace pBlocks f nodes in
      Set.iter add_edge flow) funcs in
    graph
end

module Make_inter_cfg(Expr : sig
    type expr
    val expr_to_string : expr -> string
  end) = struct
  include Make_common(Expr)

  type t = {
    blocks: (int, V.t) Hashtbl.t;
    flow: G.t;
    functions: (int, string) Hashtbl.t;
    mutable extremals: int list;
    mutable extremalsR: int list;
    mutable interflow: (int * int * int * int) list
  }

  let create () = {
    blocks= Hashtbl.create 10; flow= G.create(); functions= Hashtbl.create 10;
    extremals= []; extremalsR = []; interflow = []
  }
  let inflow { flow=g; _ } = Wrapper.inflow g
  let outflow { flow=g; _ } = Wrapper.outflow g
  let is_extremal t = Wrapper.is_extremal t.extremals
  let is_extremalR t = Wrapper.is_extremal t.extremalsR
  let get t = Wrapper.get t.blocks
  let add { blocks=b; flow=g; functions=p; _ } = Wrapper.add b g p
  let connect { flow=g; _ } ?(label = E.default) = Wrapper.connect g label
  let get_blocks { blocks=b; _ } = b
  let get_func_id { functions=p; _ } = Hashtbl.find p
  let extremal t l = t.extremals <- l::t.extremals
  let extremalR t l = t.extremalsR <- l::t.extremalsR
  let interflow t l = t.interflow <- l::t.interflow
  let inter_flow t = t.interflow

  let dot_output { blocks=b; flow=g; functions=p; _ } = Wrapper.dot_output b g p

  let display_with_gv { blocks=b; flow=g; functions=p; _ } =
    Wrapper.display_with_gv b g p

  let show = display_with_gv
end
