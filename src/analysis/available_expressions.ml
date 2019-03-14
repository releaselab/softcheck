open Batteries
open Set.Infix
open Softcheck

module Make(E : sig type expr end)
    (Printer : Sig.Printer with type expr = E.expr)
    (Cfg : Sig.Flow_graph with type expr = E.expr)
    (S : sig
       val containst_lv : E.expr -> E.expr -> bool
       val aexp_star : Cfg.program -> E.expr Set.t
       val get_non_trivial_subexpressions : E.expr Set.t -> E.expr -> E.expr Set.t
     end) = struct
  module Solve(P : sig val p : Cfg.program end) = struct
    let aexp_star = S.aexp_star P.p
    module L = Lattices.Reverse_powerset_lattice(struct
        type t = E.expr
        let bottom = aexp_star
        let to_string = Printer.expr_to_string
      end)

    let aexp =
      let open Cfg_node in
      function
        Cfg_assign (_, e) 
      | Cfg_guard e -> S.get_non_trivial_subexpressions Set.empty e
      | Cfg_call (f, args) ->
          let acc = S.get_non_trivial_subexpressions Set.empty f in
          List.fold_left S.get_non_trivial_subexpressions acc args
      | Cfg_var_decl _ | Cfg_jump _ -> Set.empty

    let kill aexp_star =
      let open Cfg_node in
      function
        Cfg_assign (lv, _) -> Set.filter (S.containst_lv lv) aexp_star
      | Cfg_call _ | Cfg_guard _ | Cfg_jump _ | Cfg_var_decl _ -> Set.empty

    let gen s =
      let open Cfg_node in
      match s with
        Cfg_assign (lv, _) -> Set.filter (not % S.containst_lv lv) (aexp s)
      | Cfg_call _ | Cfg_guard _ | Cfg_jump _ | Cfg_var_decl _ -> aexp s

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ n s =
        let b = n.Cfg_node.stmt in
        let g = gen b in
        let k = kill aexp_star b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Dataflow.Forward.Make_solution(L)(Cfg)(F)(P)
  end
end
