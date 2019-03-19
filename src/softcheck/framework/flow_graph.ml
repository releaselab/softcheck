module Make_common(Sl : Softlang.S)(N : Node_sig.S with type expr = Sl.expr)
    (C : sig type program end) =
struct
  type program = C.program
  type vertex = N.stmt N.t
  type expr = N.expr
  type edge_label = Normal | If_true | If_false

  module V = struct
    type t = vertex
    let compare x y = compare x.N.id y.N.id
    let hash x = Hashtbl.hash x.N.id
    let equal x y = x.N.id = y.N.id
  end

  module E = struct
    type t = edge_label
    let compare = compare
    let default = Normal

    (*let to_string = function
        Normal  -> ""
      | If_true   -> "true"
      | If_false    -> "false"*)
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(V)(E)

  module Display(X : sig
      val label_to_subgraph : vertex -> Graph.Graphviz.DotAttributes.subgraph
      val label_to_dot_label : vertex -> string end) = struct
    include G

    let vertex_name v = string_of_int v.N.id
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape(`Box); `Fontname("Courier")]
    let vertex_attributes v = [`Label(X.label_to_dot_label v)]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph v = Some(X.label_to_subgraph v)
  end

  module Wrapper = struct
    let inflow = G.pred

    let outflow = G.succ

    let is_extremal exts l = List.mem l exts

    let add g p func_id v =
      let () = Hashtbl.replace p v func_id in
      G.add_vertex g v

    let connect g label l l' = G.add_edge_e g (G.E.create l label l')

    let dot_output _ g p f =
      let module Helper =
      struct
        let label_to_dot_label n =
          Printf.sprintf "[%s]^%d" (N.to_string n) n.N.id
        let label_to_subgraph n =
          let fid = Hashtbl.find p n in
          { Graph.Graphviz.DotAttributes.sg_name=fid;
            sg_attributes=[`Label fid]; sg_parent=None }
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
      ignore(Sys.command ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^
                          tmp_ps ^ " &"));
      Sys.remove tmp_dot
  end
end

module Make_cfg(Sl : Softlang.S)(N : Node_sig.S with type expr = Sl.expr)
    (F : Sig.Flow with type block = Sl.stmt Sl.t and type vertex = N.stmt N.t)
    (C : sig
       type program
       type func = Sl.ident * Sl.ident list * Sl.stmt Sl.t

       val funcs : program -> func list
       val global_decls : program -> N.stmt N.t list
     end) = struct
  open Batteries

  include Make_common(Sl)(N)(C)

  type t = {
    mutable blocks: vertex Set.t;
    flow: G.t;
    functions: (vertex, string) Hashtbl.t;
    mutable extremals: vertex list;
    mutable extremalsR: vertex list
  }

  let create () = {
    blocks = Set.empty; flow = G.create(); functions= Hashtbl.create 10;
    extremals= []; extremalsR = []
  }
  let inflow { flow = g; _ } = Wrapper.inflow g
  let outflow { flow = g; _ } = Wrapper.outflow g
  let is_extremal t = Wrapper.is_extremal t.extremals
  let is_extremalR t = Wrapper.is_extremal t.extremalsR
  let add t func_id v =
    let () = t.blocks <- Set.add v t.blocks in
    Wrapper.add t.flow t.functions func_id v
  let connect { flow = g; _ } ?(label = E.default) = Wrapper.connect g label
  let get_blocks { blocks = b; _ } = b
  let get_func_id { functions = p; _ } = Hashtbl.find p
  let extremal t l = t.extremals <- l::t.extremals
  let extremalR t l = t.extremalsR <- l::t.extremalsR

  let dot_output { blocks = b; flow = g; functions = p; _ } =
    Wrapper.dot_output b g p

  let display_with_gv { blocks = b; flow = g; functions = p; _ } =
    Wrapper.display_with_gv b g p

  let show = display_with_gv

  let generate_from_program p =
    let open Batteries in
    let graph = create() in
    let pBlocks = Hashtbl.create 10 in
    let global_decls = C.global_decls p  in
    let funcs = C.funcs p in
    let () =
      Hashtbl.iter (fun id blocks -> Set.iter (add graph id) blocks) pBlocks in
    let add_edge (i, j) = connect graph i j in
    let rec aux = function
        h_1 :: h_2 :: t ->
          let () = add_edge (h_1, h_2) in
          aux (h_2 :: t)
      | [h] -> Some h
      | [] -> None in
    let last_decl = aux global_decls in
    let () = List.iter (fun (f, _, b) ->
      let { F.correspondence = ht; initial; nodes; flow; _ } = F.flow b in
      let init = Hashtbl.find ht (F.init b) in
      let () = extremal graph init in
      let finals = Set.map (Hashtbl.find ht) (F.final b) in
      let () = Set.iter (extremal graph) finals in
      let () = Hashtbl.replace pBlocks f nodes in
      let () = match last_decl with
          Some l -> Set.iter (fun i -> add_edge (l, i)) initial
        | None -> () in
      Set.iter add_edge flow) funcs in
    graph
end

module Make_inter_cfg(Sl : Softlang.S)(N : Node_sig.S with type expr = Sl.expr)
    (F : Sig.Flow with type block = Sl.stmt Sl.t and type vertex = N.stmt N.t)
    (C : sig
       type program
     end) = struct
  open Batteries

  include Make_common(Sl)(N)(C)

  type t = {
    mutable blocks: vertex Set.t;
    flow: G.t;
    functions: (vertex, string) Hashtbl.t;
    mutable extremals: vertex list;
    mutable extremalsR: vertex list;
    mutable interflow: (vertex * vertex * vertex * vertex) list
  }

  let create () = {
    blocks = Set.empty; flow = G.create(); functions = Hashtbl.create 10;
    extremals = []; extremalsR = []; interflow = []
  }
  let inflow { flow=g; _ } = Wrapper.inflow g
  let outflow { flow=g; _ } = Wrapper.outflow g
  let is_extremal t = Wrapper.is_extremal t.extremals
  let is_extremalR t = Wrapper.is_extremal t.extremalsR
  let add t func_id v =
    let () = t.blocks <- Set.add v t.blocks in
    Wrapper.add t.flow t.functions func_id v
  let connect { flow=g; _ } ?(label = E.default) = Wrapper.connect g label
  let get_blocks { blocks=b; _ } = b
  let get_func_id { functions=p; _ } = Hashtbl.find p
  let extremal t l = t.extremals <- l::t.extremals
  let extremalR t l = t.extremalsR <- l::t.extremalsR
  (*let interflow t l = t.interflow <- l::t.interflow*)
  let inter_flow t = t.interflow

  let dot_output { blocks=b; flow=g; functions=p; _ } = Wrapper.dot_output b g p

  let display_with_gv { blocks=b; flow=g; functions=p; _ } =
    Wrapper.display_with_gv b g p

  let show = display_with_gv

  let generate_from_program _ = (* TODO: *) create ()
end
