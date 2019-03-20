open Batteries
open Set.Infix
open Softcheck

module Make(N : Node_sig.S)
    (Printer : Sig.Printer with type expr = N.expr)
    (Cfg : Sig.Flow_graph with type vertex = N.stmt N.t and type expr = N.expr)
    (S : sig
       val aexp : N.expr -> N.expr Set.t
       val contains_ident : N.expr -> N.expr -> bool
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

    let kill n =
      let open N in
      match get_node_data n with
        Cfg_assign (lv, _) ->
          Set.filter (S.contains_ident (get_node_data lv)) aexp_star
      | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ -> Set.empty

    let gen n =
      let open N in
      match get_node_data n with
        Cfg_assign (_, e)
      | Cfg_guard e -> S.aexp (get_node_data e)
      | Cfg_call (f, args) ->
          let f' = S.aexp (get_node_data f) in
          List.fold_left (fun acc e -> acc ||. S.aexp (get_node_data e)) f' args
      | Cfg_jump | Cfg_var_decl _ -> Set.empty

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ b s =
        let g = gen b in
        let k = kill b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Dataflow.Backward.Make_solution(L)(Cfg)(F)(P)
  end
end
