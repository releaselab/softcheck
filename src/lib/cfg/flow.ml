open Base
open Scil

module type S = sig
  type expr

  type program

  module Ast_block : Stmt.S with type expr = expr

  module Cfg_block : Cfg_node.S with type expr = expr

  module Edge : Comparable.S with type t = Ast_block.t * Ast_block.t

  type t = {
    correspondence : Cfg_block.t Map.M(Ast_block).t;
    blocks : Set.M(Ast_block).t;
    initial : Set.M(Ast_block).t;
    final : Set.M(Ast_block).t;
    flow : Set.M(Edge).t;
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
    | Scil_assign _ | Scil_call _ | Scil_if _ | Scil_if_else _ | Scil_jump _
    | Scil_var_decl _ | Scil_while _ ->
      n
    | Scil_seq (x, _) -> init x

  let final x =
    let open Ast_block in
    let rec final_rec acc n =
      match n.stmt_s with
      | Scil_assign _ | Scil_call _ | Scil_jump _ | Scil_var_decl _
      | Scil_while _ ->
        Set.add acc n
      | Scil_if (_, x) -> final_rec acc x
      | Scil_if_else (_, x, y) -> final_rec (final_rec acc x) y
      | Scil_seq (_, x) -> final_rec acc x
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
      let open Ast_block in
      let open C in
      let new_node s =
        let n' = { stmt_label = next_label (); stmt_s = s } in
        map := Map.add_exn !map ~key:n ~data:n'
      in
      match n.Ast_block.stmt_s with
      | Scil_assign (lv, rv) ->
        let () = new_node (Cfg_assign (lv, rv)) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_var_decl v ->
        let () = new_node (Cfg_var_decl v) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_call (f, args) ->
        let () = new_node (Cfg_call (f, args)) in
        let nodes' = Set.add blocks n in
        (nodes', flow)
      | Scil_seq (s_1, s_2) ->
        let init_s2 = init s_2 in
        let final_s1 = final s_1 in
        let flow' =
          Set.fold
            ~f:(fun acc b -> Set.add acc (b, init_s2))
            final_s1 ~init:flow
        in
        flow_rec (flow_rec (blocks, flow') s_1) s_2
      | Scil_while (e, b) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Set.add blocks n in
        let flow' =
          Set.fold (final b)
            ~f:(fun acc b -> Set.add acc (b, n))
            ~init:(Set.add flow (n, init b))
        in
        flow_rec (nodes', flow') b
      | Scil_if (e, b) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Set.add blocks n in
        let flow' = Set.add flow (n, init b) in
        flow_rec (nodes', flow') b
      | Scil_if_else (e, x, y) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Set.add blocks n in
        let flow' = Set.add (Set.add flow (n, init y)) (n, init x) in
        flow_rec (flow_rec (nodes', flow') x) y
      | Scil_jump _ -> (blocks, flow)
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
