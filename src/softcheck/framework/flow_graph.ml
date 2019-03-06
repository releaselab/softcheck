module Node = Cfg_node.Processed

module Make_common(F : sig
    type expr
    val expr_to_string : expr -> string
  end) = struct
  module V = struct
    type t = F.expr Node.t
    let compare x y = compare x.Node.id y.Node.id
    let hash x = Hashtbl.hash x.Node.id
    let equal x y = x.Node.id = y.Node.id
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

    let vertex_name v = string_of_int v.Node.id
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape(`Box); `Fontname("Courier")]
    let vertex_attributes v = [`Label(X.label_to_dot_label v.Node.id)]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph v = Some(X.label_to_subgraph v.Node.id)
  end

  module Wrapper = struct
    let inflow = G.pred

    let outflow = G.succ

    let is_extremal exts l = List.mem l exts

    let get = Hashtbl.find

    let add b g p func_id v =
      let open Node in
      Hashtbl.replace b v.id v;
      Hashtbl.replace p v.id func_id; G.add_vertex g v

    let connect g label l l' = G.add_edge_e g (G.E.create l label l')

    let dot_output b g p f =
      let module Helper =
      struct
        let label_to_dot_label l =
          let n = Hashtbl.find b l in
          Printf.sprintf "[%s]^%d" (Node.to_string F.expr_to_string (n.Node.stmt)) l
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

module Make_cfg(F : sig
    type expr
    val expr_to_string : expr -> string
  end) = struct
  include Make_common(F)

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


end

module Make_inter_cfg(F : sig
    type expr
    val expr_to_string : expr -> string
  end) = struct
  include Make_common(F)

  module Cfg = Make_cfg(F)

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
