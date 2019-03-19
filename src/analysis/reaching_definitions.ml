open Batteries
open Set.Infix
open Softcheck

module type Language_component = sig
  type vertex  
  type blocks = vertex Set.t
  type definition_location = string * vertex option

  type expr
  val is_ident : expr -> bool
  val ident_of_expr : expr -> string
  val free_variables : expr -> string Set.t
end

module Make(N : Node_sig.S)
    (Cfg : Sig.Flow_graph with type vertex = N.stmt N.t)
    (S : Language_component with type vertex = Cfg.vertex and type expr = N.expr)
= struct
  module Solve(P : sig val p : Cfg.program end) = struct
    let graph = Cfg.generate_from_program P.p
    let blocks = Cfg.get_blocks graph

    module L = Lattices.Powerset_lattice(struct
        type t = S.definition_location
        let to_string (v, n) = Printf.sprintf "(%s,%s)" v (match n with
            None    -> "?"
          | Some n' -> string_of_int (n'.N.id))
      end)

    module Spec = Node_specifics.Make(N)

    let kill blocks n =
      let pair x y = (x, y) in
      let open N in
      match get_node_data n with
        Cfg_assign (lv, _) when S.is_ident (get_node_data lv) ->
          let lv' = S.ident_of_expr (get_node_data lv) in
          Set.map (pair lv' % Option.some) (Spec.find_assignments blocks lv) <-- (lv', None)
      | Cfg_assign _ | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _->
          Set.empty

    let gen n =
      let open N in
      match get_node_data n with
        Cfg_assign (lv, _) when S.is_ident (get_node_data lv) ->
          let lv' = S.ident_of_expr (get_node_data lv) in
          Set.singleton (lv', Some n)
      | Cfg_assign _ | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ ->
          Set.empty

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ n s =
        let g = gen n in
        let k = kill blocks n in
        (s -. k) ||. g

      let initial_state =
        Set.map (fun x -> x, None) (Spec.free_variables S.free_variables blocks)
    end

    module Fix = Solvers.Make_fix(L)(Cfg)(F)(Dependencies.Forward(Cfg))

    let solution = Fix.solve graph

    let get_entry_result l = solution (Fix.Circ l)
    let get_exit_result l = solution (Fix.Bullet l)
    let result_to_string = L.to_string
  end
end
