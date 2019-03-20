open Batteries
open Set.Infix
open Softcheck

module Make(N : Node_sig.S)
    (Cfg : Sig.Flow_graph with type vertex = N.stmt N.t and type expr = N.expr)
    (Printer : Sig.Printer with type expr = N.expr)
    (S : sig
       val containst_lv : N.expr -> N.expr -> bool
       val aexp : N.expr -> N.expr Set.t
     end) = struct
  module Solve(P : sig val graph : Cfg.t end) = struct
    module Spec = Node_specifics.Make(N)

    let aexp_star =
      let blocks =
        Hashtbl.fold (fun _ -> Set.add) (Cfg.get_blocks P.graph) Set.empty in
      Set.fold (fun b acc -> Spec.aexp S.aexp b ||. acc) blocks Set.empty

    module L = Lattices.Reverse_powerset_lattice(struct
        type t = N.expr
        let bottom = aexp_star
        let to_string = Printer.expr_to_string
      end)

    let kill aexp_star n =
      let open N in
      match get_node_data n with
        Cfg_assign (lv, _) -> Set.filter (S.containst_lv (get_node_data lv)) aexp_star
      | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ -> Set.empty

    let gen n =
      let open N in
      match get_node_data n with
        Cfg_assign (lv, _) ->
          Set.filter (not % S.containst_lv (get_node_data lv)) (Spec.aexp S.aexp n)
      | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ -> Spec.aexp S.aexp n

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ b s =
        let g = gen b in
        let k = kill aexp_star b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Dataflow.Forward.Make_solution(L)(Cfg)(F)(P)
  end
end
