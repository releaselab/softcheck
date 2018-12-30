module Make_common(F : sig
    type label = int
    type block
  end) = struct
  module V = struct
    type t = F.label
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

  module E = struct
    type t = Normal | IfYes | IfNo
    let compare = compare
    let default = Normal

    let to_string = function
        Normal  -> ""
      | IfYes   -> "yes"
      | IfNo    -> "no"
  end

  module G = Graph.Imperative.Digraph.ConcreteLabeled(V)(E)

  type stmt_label = F.label
  type vertex = F.block
  type edge_label = E.t
  type ident = string

  module Display(X : sig
      val label_to_subgraph : int -> Graph.Graphviz.DotAttributes.subgraph
      val label_to_dot_label : int -> string end) = struct
    include G

    let vertex_name v = string_of_int v
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape(`Box); `Fontname("Courier")]
    let vertex_attributes l = [`Label(X.label_to_dot_label l)]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph l = Some(X.label_to_subgraph l)
  end
end

module MakeCfg(F : sig
    type label = int
    type block
    val to_string : block -> string
  end) = struct
  include Make_common(F)

  type t = {
    blocks: (stmt_label, vertex) Hashtbl.t;
    flow: G.t;
    functions: (stmt_label, ident) Hashtbl.t;
    mutable extremals: stmt_label list;
    mutable extremalsR: stmt_label list
  }

  let create () = {blocks= Hashtbl.create 10; flow= G.create(); functions= Hashtbl.create 10; extremals= []; extremalsR = []}
  let inflow {flow=g; _ } l = G.pred g l
  let outflow {flow=g; _} l = G.succ g l
  let extremal t l = t.extremals <- l::t.extremals
  let is_extremal t l = List.mem l t.extremals
  let extremalR t l = t.extremalsR <- l::t.extremalsR
  let is_extremalR t l = List.mem l t.extremalsR
  let get t l = Hashtbl.find t.blocks l
  let add {blocks=b; flow=g; functions=p; _} id (l, v) = Hashtbl.replace b l v; Hashtbl.replace p l id; G.add_vertex g l
  let connect {flow=g; _} ?(label = E.default) l l' = G.add_edge_e g (G.E.create l label l')
  let get_blocks {blocks=b; _} = b
  let get_func_id {functions=p; _} l = Hashtbl.find p l

  let dot_output {blocks=b; flow=g; functions=p; _} f =
    let module Helper =
    struct
      let label_to_dot_label l = Printf.sprintf "[%s]^%d" (F.to_string(Hashtbl.find b l)) l
      let label_to_subgraph l =
        let fid = Hashtbl.find p l in
        {Graph.Graphviz.DotAttributes.sg_name=fid; sg_attributes=[`Label fid]; sg_parent=None}
    end
    in
    let module Dot_ = Graph.Graphviz.Dot(Display(Helper)) in
    let oc = open_out f in
    Dot_.output_graph oc g;
    close_out oc

  let display_with_gv g =
    let tmp_dot = Filename.temp_file "graph" ".dot" in
    dot_output g tmp_dot;
    let tmp_ps = Filename.temp_file "graph" ".ps" in
    ignore(Sys.command ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^ tmp_ps ^ " &"));
    Sys.remove tmp_dot

  let show = display_with_gv
end

module MakeInterCfg(F : sig
    type label = int
    type block
    val to_string : block -> string
  end) = struct
  include Make_common(F)

  type t = {
    blocks: (stmt_label, vertex) Hashtbl.t;
    flow: G.t;
    functions: (stmt_label, ident) Hashtbl.t;
    mutable extremals: stmt_label list;
    mutable extremalsR: stmt_label list;
    mutable interflow: (stmt_label * stmt_label * stmt_label * stmt_label) list
  }

  let create () = {blocks= Hashtbl.create 10; flow= G.create();
                   functions= Hashtbl.create 10; extremals= []; extremalsR = [];
                   interflow = []}
  let inflow {flow=g; _} l = G.pred g l
  let outflow {flow=g; _} l = G.succ g l
  let extremal t l = t.extremals <- l::t.extremals
  let interflow t l = t.interflow <- l::t.interflow
  let inter_flow t = t.interflow
  let is_extremal t l = List.mem l t.extremals
  let extremalR t l = t.extremalsR <- l::t.extremalsR
  let is_extremalR t l = List.mem l t.extremalsR
  let get t l = Hashtbl.find t.blocks l
  let add {blocks=b; flow=g; functions=p; _} id (l, v) = Hashtbl.replace b l v; Hashtbl.replace p l id; G.add_vertex g l
  let connect {flow=g; _} ?(label = E.default) l l' = G.add_edge_e g (G.E.create l label l')
  let get_blocks {blocks=b; _} = b
  let get_func_id {functions=p; _} l = Hashtbl.find p l

  let dot_output {blocks=b; flow=g; functions=p; _} f =
    let module Helper =
    struct
      let label_to_dot_label l = Printf.sprintf "[%s]^%d" (F.to_string(Hashtbl.find b l)) l
      let label_to_subgraph l =
        let fid = Hashtbl.find p l in
        {Graph.Graphviz.DotAttributes.sg_name=fid; sg_attributes=[`Label fid]; sg_parent=None}
    end
    in
    let module Dot_ = Graph.Graphviz.Dot(Display(Helper)) in
    let oc = open_out f in
    Dot_.output_graph oc g;
    close_out oc

  let display_with_gv g =
    let tmp_dot = Filename.temp_file "graph" ".dot" in
    dot_output g tmp_dot;
    let tmp_ps = Filename.temp_file "graph" ".ps" in
    ignore(Sys.command ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^ tmp_ps ^ " &"));
    Sys.remove tmp_dot

  let show = display_with_gv
end
