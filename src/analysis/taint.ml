open Batteries
open Set.Infix
open Softcheck

module Make(N : Node_sig.S)(Cfg : Sig.Flow_graph with type vertex = N.stmt N.t
                                                  and type expr = N.expr)
    (S : sig
       include Reaching_definitions.Language_component

       val eval : (string, Taint_lattice.property) Map.t -> expr ->
         Taint_lattice.property
     end with type vertex = Cfg.vertex and type expr = N.expr) = struct
  module RD_S = Reaching_definitions.Make(N)(Cfg)(S)
  module Solve(P : sig val p : Cfg.program end) = struct
    module RD = RD_S.Solve(P)

    let graph = Cfg.generate_from_program P.p
    let blocks = Cfg.get_blocks graph
    let vars = RD.Spec.free_variables S.free_variables blocks

    module Var_tainting_lattice = Lattices.Map_lattice(struct
        type t = string
        let to_string = identity
        let bottom_elems = vars
      end)(Taint_lattice)

    module Reaching_definitions_lattice = Lattices.Powerset_lattice(struct
        type t = S.definition_location
        let to_string (v, n) = Printf.sprintf "(%s,%s)" v (match n with
            None    -> "?"
          | Some n' -> string_of_int n'.N.id)
      end)

    module L = Lattices.Pair_lattice(Reaching_definitions_lattice)(Var_tainting_lattice)

    let ta s n =
      let open N in
      match get_node_data n with
        Cfg_assign (lv, rv) when S.is_ident (get_node_data lv) ->
          let eval_rv = S.eval s (get_node_data rv) in
          [S.ident_of_expr (get_node_data lv), eval_rv]
      | Cfg_assign _ | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ ->
          []

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ b s =
        let g = RD.gen b in
        let k = RD.kill blocks b in
        let s1 = (fst s -. k) ||. g in
        let new_tv = ta (snd s) b in
        let s2 = List.fold_left (fun m (i,eval) -> Var_tainting_lattice.set m i eval) (snd s) new_tv in
        s1, s2

      let initial_state =
        Set.map (fun x -> x,None) vars,
        Set.fold (fun x acc -> Map.add x Taint_lattice.bottom acc) vars Map.empty
    end

    include Dataflow.Forward.Make_solution(L)(Cfg)(F)(P)
  end
end
