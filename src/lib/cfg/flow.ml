open! Core
open Scil

module type S = sig
  type expr
  type program

  module Ast_block : Stmt.S with type expr = expr
  module Cfg_block : Cfg_node.S with type expr = expr
  module Edge : Comparable.S with type t = Ast_block.t * Ast_block.t

  type t = {
    correspondence : Cfg_block.t Ast_block.Map.t;
    blocks : Ast_block.Set.t;
    initial : Ast_block.Set.t;
    final : Ast_block.Set.t;
    flow : Edge.Set.t;
  }

  val init : Ast_block.t -> Ast_block.t
  val final : Ast_block.t -> Set.M(Ast_block).t
  val flow : Ast_block.t -> t
  val flowR : Ast_block.t -> t
end

module Make
  (E : Expr.S)
  (S : Softlang.S with type expr := E.t)
  (C : Cfg_node.S with type expr = E.t) =
struct
  (* FIXME: check jumps *)
  type expr = E.t
  type program = S.program

  module Ast_block = S.Stmt
  module Cfg_block = C

  module Edge = struct
    module T = struct
      type t = Ast_block.t * Ast_block.t [@@deriving ord, sexp]
    end

    include T
    include Comparable.Make (T)
  end

  type t = {
    correspondence : Cfg_block.t Map.M(Ast_block).t;
    blocks : Set.M(Ast_block).t;
    initial : Set.M(Ast_block).t;
    final : Set.M(Ast_block).t;
    flow : Set.M(Edge).t;
  }

  let rec init n =
    let open Ast_block in
    match n.stmt_s with
    | Scil_assign _ | Scil_fun_decl _ | Scil_fun_call _ | Scil_fun_call_assign _
    | Scil_if_else _ | Scil_goto _ | Scil_var_decl _ | Scil_while _
    | Scil_seq Seq_skip
    | Scil_fun_call_var_assign _ | Scil_var_assign _ | Scil_return _
    | Scil_label _ ->
      n
    | Scil_seq (Seq_cons (x, _)) -> init x

  let final x =
    let rec final_rec acc n =
      let open Ast_block in
      match n.stmt_s with
      | Scil_assign _ | Scil_fun_decl _ | Scil_fun_call _
      | Scil_fun_call_assign _ | Scil_var_decl _ | Scil_while _
      | Scil_var_assign _ | Scil_return _ | Scil_label _
      | Scil_seq Seq_skip
      | Scil_fun_call_var_assign _ | Scil_goto _ ->
        Set.add acc n
      | Scil_if_else (_, x, y) -> final_rec (final_rec acc x) y
      | Scil_seq seq -> seq_final acc seq
    and seq_final acc = function
      | Seq_cons (x, Seq_skip) -> final_rec acc x
      | Seq_cons (_, (Seq_cons _ as x)) -> seq_final acc x
      | Seq_skip -> assert false
    in
    final_rec (Set.empty (module Ast_block)) x

  let flow x =
    let map = ref (Map.empty (module Ast_block)) in
    let counter = ref (-1) in
    let next_label () =
      let () = counter := !counter + 1 in
      !counter
    in
    let rec flow_rec (blocks, flow) n =
      let new_node s =
        let n' = { C.stmt_label = next_label (); stmt_s = s } in
        map := Map.add_exn !map ~key:n ~data:n'
      in
      let rec seq_flow (blocks, flow) = function
        | Ast_block.Seq_cons (s, Seq_skip) -> flow_rec (blocks, flow) s
        | Seq_cons (s_1, (Seq_cons (s_2, _) as seq)) ->
          let init_s2 = init s_2 in
          let final_s1 = final s_1 in
          let flow' =
            Set.fold
              ~f:(fun acc b -> Set.add acc (b, init_s2))
              final_s1 ~init:flow
          in
          seq_flow (blocks, flow') seq
        | Seq_skip -> (blocks, flow)
      in
      match n.Ast_block.stmt_s with
      | Scil_assign (lv, rv) ->
        let () = new_node (Cfg_assign (lv, rv)) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_var_assign (lv, rv) ->
        let () = new_node (Cfg_var_assign (lv, rv)) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_var_decl v ->
        let () = new_node (Cfg_var_decl v) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_fun_decl (_, _, _) -> (* TODO *) (blocks, flow)
      | Scil_fun_call (f, args) ->
        let () = new_node (Cfg_call (f, args)) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_fun_call_assign (v, f, args) ->
        let () = new_node (Cfg_call_assign (v, f, args)) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_fun_call_var_assign (v, f, args) ->
        let () = new_node (Cfg_call_var_assign (v, f, args)) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_seq seq -> seq_flow (blocks, flow) seq
      | Scil_while (e, b) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Set.add blocks n in
        let flow' =
          Set.fold (final b)
            ~f:(fun acc b -> Set.add acc (b, n))
            ~init:(Set.add flow (n, init b))
        in
        flow_rec (nodes', flow') b
      | Scil_if_else (e, x, y) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Set.add blocks n in
        let flow' = Set.add (Set.add flow (n, init y)) (n, init x) in
        flow_rec (flow_rec (nodes', flow') x) y
      | Scil_label (_, s) -> flow_rec (blocks, flow) s
      | Scil_goto _ -> (blocks, flow)
      | Scil_return e ->
        let () = new_node (Cfg_return e) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
    in
    let blocks, flow =
      flow_rec (Set.empty (module Ast_block), Set.empty (module Edge)) x
    in
    let initial = Set.singleton (module Ast_block) (init x) in
    let final = final x in
    { correspondence = !map; initial; final; blocks; flow }

  let flowR n =
    let f = flow n in
    let flow = Set.map (module Edge) f.flow ~f:(fun (a, b) -> (b, a)) in
    { f with initial = f.final; final = f.initial; flow }
end
