open Core
open Scil

module type FlowGraph = sig
  type block

  module Vertex = Label

  type edge_label = Normal | If_true | If_false
  type program
  type t

  val create : unit -> t
  val inflow : t -> Vertex.t -> Vertex.t list
  val outflow : t -> Vertex.t -> Vertex.t list
  val is_extremal : t -> Vertex.t -> bool
  val is_extremalR : t -> Vertex.t -> bool
  val add : t -> string -> block -> unit
  val get : t -> Vertex.t -> block
  val connect : t -> ?label:edge_label -> Vertex.t -> Vertex.t -> unit
  val get_blocks : t -> (Vertex.t, block) Hashtbl.t
  val get_func_id : t -> Vertex.t -> string
  val extremal : t -> Vertex.t -> unit
  val extremalR : t -> Vertex.t -> unit
  val labels : t -> Set.M(Label).t
  val dot_output : t -> string -> unit
  val display_with_gv : t -> unit
  val show : t -> unit
  val generate_from_program : program -> t
end

module type InterFlowGraph = sig
  include FlowGraph

  val inter_flow : t -> (Vertex.t * Vertex.t * Vertex.t * Vertex.t) list
  (* TODO: val callees : t -> int -> int list *)
end

module Make_common
  (E : Expr.S)
  (S : Softlang.S with type expr = E.t)
  (C : Cfg_node.S with type expr = E.t)
  (F : Flow.S
         with type expr = E.t
          and type program = S.program
          and module Ast_block = S.Stmt
          and module Cfg_block = C) =
struct
  module V = struct
    include Label

    let hash = Hashtbl.hash
  end

  module Vertex = Label

  type block = F.Cfg_block.t
  type edge_label = Normal | If_true | If_false [@@deriving ord]

  module E = struct
    type t = edge_label [@@deriving ord]

    let default = Normal

    (*let to_string = function
        Normal  -> ""
      | If_true   -> "true"
      | If_false    -> "false"*)
  end

  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (V) (E)

  module Display (X : sig
    val label_to_subgraph : Vertex.t -> Graph.Graphviz.DotAttributes.subgraph
    val label_to_dot_label : Vertex.t -> string
  end) =
  struct
    include G

    let vertex_name = Label.to_string
    let graph_attributes _ = []
    let default_vertex_attributes _ = [ `Shape `Box; `Fontname "Courier" ]
    let vertex_attributes v = [ `Label (X.label_to_dot_label v) ]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph v = Some (X.label_to_subgraph v)
  end

  module Wrapper = struct
    let inflow g n = G.pred g n
    let outflow g n = G.succ g n
    let is_extremal exts l = List.mem exts l ~equal:V.equal

    let add g b_ht f_ht func_id n =
      let () = Hashtbl.add_exn b_ht ~key:n.C.stmt_label ~data:n in
      let () = Hashtbl.add_exn f_ht ~key:n.C.stmt_label ~data:func_id in
      G.add_vertex g n.C.stmt_label

    let connect g label l l' = G.add_edge_e g (G.E.create l label l')

    let dot_output b_ht f_ht g f =
      let module Helper = struct
        let label_to_dot_label l =
          let n = Hashtbl.find_exn b_ht l in
          C.to_string n

        let label_to_subgraph l =
          let fid = Hashtbl.find_exn f_ht l in
          {
            Graph.Graphviz.DotAttributes.sg_name = fid;
            sg_attributes = [ `Label fid ];
            sg_parent = None;
          }
      end in
      let module Dot_ = Graph.Graphviz.Dot (Display (Helper)) in
      let oc = Stdio.Out_channel.create f in
      Dot_.output_graph oc g;
      Out_channel.close oc

    let display_with_gv b g p =
      let tmp_dot = Filename_unix.temp_file "graph" ".dot" in
      dot_output b g p tmp_dot;
      let tmp_ps = Filename_unix.temp_file "graph" ".ps" in
      ignore
        (Sys_unix.command
           ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^ tmp_ps ^ " &"));
      Sys_unix.remove tmp_dot
  end
end

module Make_cfg
  (E : Expr.S)
  (S : Softlang.S with type expr = E.t)
  (C : Cfg_node.S with type expr = E.t)
  (F : Flow.S
         with type expr = E.t
          and type program = S.program
          and module Ast_block = S.Stmt
          and module Cfg_block = C)
  (P : sig
    type t

    val convert_program_to_sl : t -> S.program
  end) =
struct
  include Make_common (E) (S) (C) (F)

  type program = P.t

  type t = {
    blocks : (Vertex.t, block) Hashtbl.t;
    graph : G.t;
    functions : (Vertex.t, string) Hashtbl.t;
    mutable extremals : Vertex.t list;
    mutable extremalsR : Vertex.t list;
  }

  let create () =
    {
      blocks = Hashtbl.create (module V);
      graph = G.create ();
      functions = Hashtbl.create (module V);
      extremals = [];
      extremalsR = [];
    }

  let get t = Hashtbl.find_exn t.blocks
  let inflow g = Wrapper.inflow g.graph
  let outflow g = Wrapper.outflow g.graph
  let is_extremal g = Wrapper.is_extremal g.extremals
  let is_extremalR g = Wrapper.is_extremal g.extremalsR
  let add t func_id n = Wrapper.add t.graph t.blocks t.functions func_id n
  let connect { graph; _ } ?(label = E.default) = Wrapper.connect graph label
  let get_blocks { blocks = b; _ } = b
  let get_func_id { functions = p; _ } = Hashtbl.find_exn p
  let extremal t l = t.extremals <- l :: t.extremals
  let extremalR t l = t.extremalsR <- l :: t.extremalsR

  let labels { blocks; _ } =
    Hashtbl.fold
      ~f:(fun ~key:l ~data:_ acc -> Set.add acc l)
      blocks
      ~init:(Set.empty (module V))

  let dot_output { blocks = b; graph = g; functions = p; _ } =
    Wrapper.dot_output b p g

  let display_with_gv { blocks = b; graph = g; functions = p; _ } =
    Wrapper.display_with_gv b p g

  let show = display_with_gv

  let generate_from_program p =
    let graph = create () in
    let _global_decls, funcs = P.convert_program_to_sl p in
    let add_edge (i, j) = connect graph i j in
    let pBlocks = Hashtbl.create (module String) in
    (* let rec aux acc = function
         | h_1 :: h_2 :: t ->
           let n_1 =
             { C.stmt_label = h_1.Decl.decl_location; C.stmt_s = Cfg_var_decl h_1 }
           in
           let n_2 =
             { C.stmt_label = h_2.Decl.decl_location; C.stmt_s = Cfg_var_decl h_2 }
           in
           let () = add_edge (n_1.stmt_label, n_2.stmt_label) in
           aux (n_1 :: acc) (h_2 :: t)
         | [ h ] ->
           let n =
             { C.stmt_label = h.Decl.decl_location; C.stmt_s = Cfg_var_decl h }
           in
           (n :: acc, Some n)
         | [] -> (acc, None)
       in
       let ns, last_decl = aux [] global_decls in
       let () = List.iter ns ~f:(add graph "main") in *)
    let () =
      List.iter
        ~f:(fun (f, _, b) ->
          let { F.correspondence = m; initial = _; blocks; flow; _ } =
            F.flow b
          in
          let () =
            Set.iter ~f:(fun b -> add graph f (Map.find_exn m b)) blocks
          in
          let () =
            let init = Map.find_exn m (F.init b) in
            extremal graph init.stmt_label
          in
          let () =
            Set.iter
              ~f:(fun b ->
                let final = Map.find_exn m b in

                extremal graph final.stmt_label)
              (F.final b)
          in
          let () = Hashtbl.add_exn pBlocks ~key:f ~data:blocks in
          (* let () =
               match last_decl with
               | Some l ->
                 Set.iter
                   ~f:(fun i ->
                     let i' = Map.find_exn m i in
                     add_edge (l.stmt_label, i'.stmt_label))
                   initial
               | None -> ()
             in *)
          Set.iter
            ~f:(fun (b_1, b_2) ->
              let b_1' = Map.find_exn m b_1 in
              let b_2' = Map.find_exn m b_2 in
              add_edge (b_1'.stmt_label, b_2'.stmt_label))
            flow)
        funcs
    in
    graph
end

module Make_inter_cfg
  (E : Expr.S)
  (S : Softlang.S with type expr = E.t)
  (C : Cfg_node.S with type expr = E.t)
  (F : Flow.S
         with type expr = E.t
          and type program = S.program
          and module Ast_block = S.Stmt
          and module Cfg_block = C)
  (P : sig
    type t

    (* val convert_program_to_sl : t -> S.program *)
  end) =
struct
  include Make_common (E) (S) (C) (F)

  type program = P.t

  type t = {
    blocks : (Vertex.t, block) Hashtbl.t;
    graph : G.t;
    functions : (Vertex.t, string) Hashtbl.t;
    mutable extremals : Vertex.t list;
    mutable extremalsR : Vertex.t list;
    mutable interflow : (Vertex.t * Vertex.t * Vertex.t * Vertex.t) list;
  }

  let create () =
    {
      blocks = Hashtbl.create (module V);
      graph = G.create ();
      functions = Hashtbl.create (module V);
      extremals = [];
      extremalsR = [];
      interflow = [];
    }

  let get t = Hashtbl.find_exn t.blocks
  let inflow g = Wrapper.inflow g.graph
  let outflow g = Wrapper.outflow g.graph
  let is_extremal g = Wrapper.is_extremal g.extremals
  let is_extremalR g = Wrapper.is_extremal g.extremalsR
  let add t func_id n = Wrapper.add t.graph t.blocks t.functions func_id n
  let connect { graph; _ } ?(label = E.default) = Wrapper.connect graph label
  let get_blocks { blocks = b; _ } = b
  let get_func_id { functions = p; _ } = Hashtbl.find_exn p
  let extremal t l = t.extremals <- l :: t.extremals
  let extremalR t l = t.extremalsR <- l :: t.extremalsR

  let labels { blocks; _ } =
    Hashtbl.fold
      ~f:(fun ~key:l ~data:_ acc -> Set.add acc l)
      blocks
      ~init:(Set.empty (module Label))

  let dot_output { blocks = b; graph = g; functions = p; _ } =
    Wrapper.dot_output b p g

  let display_with_gv { blocks = b; graph = g; functions = p; _ } =
    Wrapper.display_with_gv b p g

  let show = display_with_gv
  let generate_from_program _ = (* TODO: *) create ()
  let inter_flow _ = (* TODO: *) assert false
end
