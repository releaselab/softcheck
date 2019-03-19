open Batteries
open Softcheck

module Make(N : Node_sig.S)
    (Cfg : Sig.Flow_graph with type vertex = N.stmt N.t and type expr = N.expr)
    (S : sig
       val free_variables : N.expr -> N.ident Set.t
     end) = struct
  module Solve(P : sig val p : Cfg.program end) = struct
    module L = Lattices.Powerset_lattice(struct
        type t = string
        let to_string = identity
      end)

    let kill n =
      let open N in
      match get_node_data n with
        Cfg_assign (lv, _) -> S.free_variables (get_node_data lv)
      | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ -> Set.empty

    let gen n =
      let open N in
      match get_node_data n with
        Cfg_assign (_, rv) -> S.free_variables (get_node_data rv)
      | Cfg_guard e -> S.free_variables (get_node_data e)
      | Cfg_call (f, args) ->
          List.fold_left (fun acc e ->
            Set.union acc (S.free_variables (get_node_data e)))
            (S.free_variables (get_node_data f)) args
      | Cfg_jump | Cfg_var_decl _ -> Set.empty

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ b s = let open Set.Infix in
        let g = gen b in
        let k = kill b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Dataflow.Backward.Make_solution(L)(Cfg)(F)(P)
  end
end
