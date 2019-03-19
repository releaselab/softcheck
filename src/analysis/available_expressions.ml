open Batteries
open Set.Infix
open Softcheck

module Make(N : Node_sig.S)
    (Cfg : Sig.Flow_graph with type vertex = N.stmt N.t)
    (Printer : Sig.Printer with type expr = N.expr)
    (S : sig
       val containst_lv : N.expr -> N.expr -> bool
       val aexp_star : Cfg.program -> N.expr Set.t
       val get_non_trivial_subexpressions : N.expr Set.t -> N.expr -> N.expr Set.t
     end) = struct
  module Solve(P : sig val p : Cfg.program end) = struct
    let aexp_star = S.aexp_star P.p
    module L = Lattices.Reverse_powerset_lattice(struct
        type t = N.expr
        let bottom = aexp_star
        let to_string = Printer.expr_to_string
      end)

    let aexp =
      let open N in
      function
        Cfg_assign (_, e) 
      | Cfg_guard e -> S.get_non_trivial_subexpressions Set.empty (get_node_data e)
      | Cfg_call (f, args) ->
          let acc = S.get_non_trivial_subexpressions Set.empty (get_node_data f) in
          List.fold_left (fun acc x -> S.get_non_trivial_subexpressions acc (get_node_data x)) acc args
      | Cfg_var_decl _ | Cfg_jump -> Set.empty

    let kill aexp_star =
      let open N in
      function
        Cfg_assign (lv, _) -> Set.filter (S.containst_lv (get_node_data lv)) aexp_star
      | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ -> Set.empty

    let gen s =
      let open N in
      match s with
        Cfg_assign (lv, _) -> Set.filter (not % S.containst_lv (get_node_data lv)) (aexp s)
      | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ -> aexp s

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ n s =
        let b = N.get_node_data n in
        let g = gen b in
        let k = kill aexp_star b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Dataflow.Forward.Make_solution(L)(Cfg)(F)(P)
  end
end
