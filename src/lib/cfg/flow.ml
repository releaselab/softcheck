open Scil

module Make (Sl : Softlang.S) (N : Cfg_node.S with type Expr.t = Sl.Expr.t) =
struct
  (* FIXME: check jumps *)
  module Ast_block = Sl.Stmt

  type ast_block = Ast_block.t

  module Cfg_block = N.Stmt

  type cfg_block = Cfg_block.t

  module Edge = struct
    module T = struct
      type t = Ast_block.t * Ast_block.t [@@deriving eq, ord]

      let to_string (b_1, b_2) =
        [%string "(%{b_1#Ast_block}, %{b_2#Ast_block})"]
    end

    include T
    include Utils.Collections.Make (T)
  end

  type edge = Edge.t

  type t = {
    correspondence : (ast_block, cfg_block) Hashtbl.t;
    blocks : Ast_block.Set.t;
    initial : Ast_block.Set.t;
    final : Ast_block.Set.t;
    flow : Edge.Set.t;
  }

  let rec init n =
    let open Sl.Stmt in
    match n.stmt_s with
    | Scil_assign _ | Scil_call _ | Scil_if _ | Scil_if_else _ | Scil_jump _
    | Scil_var_decl _ | Scil_while _ ->
      n
    | Scil_seq (x, _) -> init x

  let final x =
    let open Sl.Stmt in
    let rec final_rec acc n =
      match n.stmt_s with
      | Scil_assign _ | Scil_call _ | Scil_jump _ | Scil_var_decl _
      | Scil_while _ ->
        Ast_block.Set.add n acc
      | Scil_if (_, x) -> final_rec acc x
      | Scil_if_else (_, x, y) -> final_rec (final_rec acc x) y
      | Scil_seq (_, x) -> final_rec acc x
    in
    final_rec Set.empty x

  let rev_pair x y = (y, x)

  let flow x =
    let ht = Hashtbl.create 10 in
    let counter = ref (-1) in
    let next_label () =
      let () = counter := !counter + 1 in
      !counter
    in
    let rec flow_rec (blocks, flow) n =
      let open Sl.Stmt in
      let open N.Stmt in
      let new_node s =
        let n' = { stmt_label = next_label (); stmt_s = s } in
        Hashtbl.add ht n n'
      in
      match n.Sl.Stmt.stmt_s with
      | Scil_assign (lv, rv) ->
        let () = new_node (Cfg_assign (lv, rv)) in
        let nodes' = Ast_block.Set.add n blocks in
        (nodes', flow)
      | Scil_var_decl v ->
        let () = new_node (Cfg_var_decl v) in
        let nodes' = Ast_block.Set.add n blocks in
        (nodes', flow)
      | Scil_call (f, args) ->
        let () = new_node (Cfg_call (f, args)) in
        let nodes' = Ast_block.Set.add n blocks in
        (nodes', flow)
      | Scil_seq (s_1, s_2) ->
        let init_s2 = init s_2 in
        let final_s1 = final s_1 in
        let flow' =
          Ast_block.Set.fold (fun b -> Edge.Set.add (b, init_s2)) final_s1 flow
        in
        flow_rec (flow_rec (blocks, flow') s_1) s_2
      | Scil_while (e, b) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Ast_block.Set.add n blocks in
        let flow' =
          Ast_block.Set.fold
            (fun b -> Edge.Set.add (b, n))
            (final b)
            (Edge.Set.add (n, init b) flow)
        in
        flow_rec (nodes', flow') b
      | Scil_if (e, b) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Ast_block.Set.add n blocks in
        let flow' = Edge.Set.add (n, init b) flow in
        flow_rec (nodes', flow') b
      | Scil_if_else (e, x, y) ->
        let () = new_node (Cfg_guard e) in
        let nodes' = Ast_block.Set.add n blocks in
        let flow' = Edge.Set.add (n, init x) (Edge.Set.add (n, init y) flow) in
        flow_rec (flow_rec (nodes', flow') x) y
      | Scil_jump _ -> (blocks, flow)
    in
    let blocks, flow = flow_rec (Ast_block.Set.empty, Edge.Set.empty) x in
    let initial = Ast_block.Set.singleton (init x) in
    let final = final x in
    { correspondence = ht; initial; final; blocks; flow }

  let flowR n =
    let f = flow n in
    let flow = Edge.Set.map (fun (a, b) -> (b, a)) f.flow in
    { f with initial = f.final; final = f.initial; flow }
end
