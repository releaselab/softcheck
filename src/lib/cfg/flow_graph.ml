module Sig_cfg = Sig
open Scil

module Make_common
  (Sl : Softlang.S)
  (N : Cfg_node.S with type Expr.t = Sl.Expr.t) (C : sig
    type program
  end) =
struct
  type expr = N.expr

  type block = N.Stmt.t

  module V = struct
    include Label

    let hash = Hashtbl.hash
  end

  type vertex = V.t

  type edge_label = Normal | If_true | If_false [@@deriving ord]

  module E = struct
    type t = edge_label [@@deriving ord]

    let default = Normal

    (*let to_string = function
        Normal  -> ""
      | If_true   -> "true"
      | If_false    -> "false"*)
  end

  type program = C.program

  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (V) (E)

  module Display (X : sig
    val label_to_subgraph : vertex -> Graph.Graphviz.DotAttributes.subgraph

    val label_to_dot_label : vertex -> string
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

    let is_extremal exts l = List.mem l exts

    let add g b_ht f_ht func_id n =
      let () = Hashtbl.add b_ht n.N.Stmt.stmt_label n in
      let () = Hashtbl.add f_ht n.N.Stmt.stmt_label func_id in
      G.add_vertex g n.N.Stmt.stmt_label

    let connect g label l l' = G.add_edge_e g (G.E.create l label l')

    let dot_output b_ht f_ht g f =
      let module Helper = struct
        let label_to_dot_label l =
          let n = Hashtbl.find b_ht l in
          N.Stmt.to_string n

        let label_to_subgraph l =
          let fid = Hashtbl.find f_ht l in
          {
            Graph.Graphviz.DotAttributes.sg_name = fid;
            sg_attributes = [ `Label fid ];
            sg_parent = None;
          }
      end in
      let module Dot_ = Graph.Graphviz.Dot (Display (Helper)) in
      let oc = open_out f in
      Dot_.output_graph oc g;
      close_out oc

    let display_with_gv b g p =
      let tmp_dot = Filename.temp_file "graph" ".dot" in
      dot_output b g p tmp_dot;
      let tmp_ps = Filename.temp_file "graph" ".ps" in
      ignore
        (Sys.command
           ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^ tmp_ps ^ " &"));
      Sys.remove tmp_dot
  end
end

module Make_cfg
  (Sl : Softlang.S)
  (N : Cfg_node.S with type Expr.t = Sl.Expr.t)
  (F : Sig_cfg.Flow
         with type Ast_block.t = Sl.Stmt.t
          and type Cfg_block.t = N.Stmt.t)
  (C : sig
    type program

    val convert_program_to_sl : program -> Sl.program
  end) =
struct
  include Make_common (Sl) (N) (C)

  type t = {
    blocks : (vertex, block) Hashtbl.t;
    graph : G.t;
    functions : (vertex, string) Hashtbl.t;
    mutable extremals : vertex list;
    mutable extremalsR : vertex list;
  }

  let create () =
    {
      blocks = Hashtbl.create 10;
      graph = G.create ();
      functions = Hashtbl.create 10;
      extremals = [];
      extremalsR = [];
    }

  let get t = Hashtbl.find t.blocks

  let inflow g = Wrapper.inflow g.graph

  let outflow g = Wrapper.outflow g.graph

  let is_extremal g = Wrapper.is_extremal g.extremals

  let is_extremalR g = Wrapper.is_extremal g.extremalsR

  let add t func_id n = Wrapper.add t.graph t.blocks t.functions func_id n

  let connect { graph; _ } ?(label = E.default) = Wrapper.connect graph label

  let get_blocks { blocks = b; _ } = b

  let get_func_id { functions = p; _ } = Hashtbl.find p

  let extremal t l = t.extremals <- l :: t.extremals

  let extremalR t l = t.extremalsR <- l :: t.extremalsR

  let labels { blocks; _ } =
    Hashtbl.fold (fun l _ -> Label.Set.add l) blocks Label.Set.empty

  let dot_output { blocks = b; graph = g; functions = p; _ } =
    Wrapper.dot_output b p g

  let display_with_gv { blocks = b; graph = g; functions = p; _ } =
    Wrapper.display_with_gv b p g

  let show = display_with_gv

  let generate_from_program p =
    let graph = create () in
    let pBlocks = Hashtbl.create 10 in
    let global_decls, funcs = C.convert_program_to_sl p in
    let add_edge (i, j) = connect graph i j in
    let rec aux acc = function
      | h_1 :: h_2 :: t ->
        let n_1 =
          {
            N.Stmt.stmt_label = h_1.Decl.decl_location;
            N.Stmt.stmt_s = Cfg_var_decl h_1;
          }
        in
        let n_2 =
          {
            N.Stmt.stmt_label = h_2.Decl.decl_location;
            N.Stmt.stmt_s = Cfg_var_decl h_2;
          }
        in
        let () = add_edge (n_1.stmt_label, n_2.stmt_label) in
        aux (n_1 :: acc) (h_2 :: t)
      | [ h ] ->
        let n =
          {
            N.Stmt.stmt_label = h.Decl.decl_location;
            N.Stmt.stmt_s = Cfg_var_decl h;
          }
        in
        (n :: acc, Some n)
      | [] -> (acc, None)
    in
    let ns, last_decl = aux [] global_decls in
    let () = List.iter (add graph "main") ns in
    let () =
      List.iter
        (fun (f, _, b) ->
          let { F.correspondence = ht; initial; blocks; flow; _ } = F.flow b in
          let () =
            F.Ast_block.Set.iter
              (fun b -> add graph f (Hashtbl.find ht b))
              blocks
          in
          let () =
            let init = Hashtbl.find ht (F.init b) in
            extremal graph init.stmt_label
          in
          let () =
            F.Ast_block.Set.iter
              (fun b ->
                let final = Hashtbl.find ht b in

                extremal graph final.stmt_label)
              (F.final b)
          in
          let () = Hashtbl.add pBlocks f blocks in
          let () =
            match last_decl with
            | Some l ->
              F.Ast_block.Set.iter
                (fun i ->
                  let i' = Hashtbl.find ht i in
                  add_edge (l.stmt_label, i'.stmt_label))
                initial
            | None -> ()
          in
          F.Edge.Set.iter
            (fun (b_1, b_2) ->
              let b_1' = Hashtbl.find ht b_1 in
              let b_2' = Hashtbl.find ht b_2 in
              add_edge (b_1'.stmt_label, b_2'.stmt_label))
            flow)
        funcs
    in
    graph
end

module Make_inter_cfg
  (Sl : Softlang.S)
  (N : Cfg_node.S with type Expr.t = Sl.Expr.t)
  (F : Sig_cfg.Flow
         with type Ast_block.t = Sl.Stmt.t
          and type Cfg_block.t = N.Stmt.t)
  (C : sig
    type program
  end) =
struct
  include Make_common (Sl) (N) (C)

  type t = {
    blocks : (vertex, block) Hashtbl.t;
    graph : G.t;
    functions : (vertex, string) Hashtbl.t;
    mutable extremals : vertex list;
    mutable extremalsR : vertex list;
    mutable interflow : (vertex * vertex * vertex * vertex) list;
  }

  let create () =
    {
      blocks = Hashtbl.create 10;
      graph = G.create ();
      functions = Hashtbl.create 10;
      extremals = [];
      extremalsR = [];
      interflow = [];
    }

  let get t = Hashtbl.find t.blocks

  let inflow g = Wrapper.inflow g.graph

  let outflow g = Wrapper.outflow g.graph

  let is_extremal g = Wrapper.is_extremal g.extremals

  let is_extremalR g = Wrapper.is_extremal g.extremalsR

  let add t func_id n = Wrapper.add t.graph t.blocks t.functions func_id n

  let connect { graph; _ } ?(label = E.default) = Wrapper.connect graph label

  let get_blocks { blocks = b; _ } = b

  let get_func_id { functions = p; _ } = Hashtbl.find p

  let extremal t l = t.extremals <- l :: t.extremals

  let extremalR t l = t.extremalsR <- l :: t.extremalsR

  let labels { blocks; _ } =
    Hashtbl.fold (fun l _ -> Label.Set.add l) blocks Label.Set.empty

  let dot_output { blocks = b; graph = g; functions = p; _ } =
    Wrapper.dot_output b p g

  let display_with_gv { blocks = b; graph = g; functions = p; _ } =
    Wrapper.display_with_gv b p g

  let show = display_with_gv

  let generate_from_program _ = (* TODO: *) create ()

  let inter_flow _ = (* TODO: *) assert false
end
