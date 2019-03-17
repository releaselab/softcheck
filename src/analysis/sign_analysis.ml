open Batteries
open Softcheck

module Make(Cfg : Sig.Flow_graph)
    (S : sig
       type expr

       val is_ident : expr -> bool
       val ident_of_expr : expr -> string
       val expr_sign_eval : (string, Sign_lattice.property) Map.t -> expr ->
         Sign_lattice.property
     end with type expr = Cfg.expr) = struct
  module Solve(P : sig val p : Cfg.program end) = struct
    let declaredVars =
      let aux ((funcs, global_vars) : Cfg.program) =
        let ht = Hashtbl.create 10 in
        let () = List.iter (fun (f, vars, _) ->
          List.fold_left (fun acc v -> Set.add v acc) Set.empty vars |>
          Hashtbl.add ht f) funcs in
        ht in
      aux P.p

    module L = Lattices.Map_lattice(struct
        type t = string
        let to_string = identity
        let bottom_elems = declaredVars
      end)(Sign_lattice)

    let sign_eval env n =
      let open Cfg_node in
      match n.stmt with
        Cfg_assign (lv,rv) when S.is_ident lv ->
          [S.ident_of_expr lv, S.expr_sign_eval env rv]
      | Cfg_var_decl v -> [v, Sign_lattice.bottom]
      | Cfg_assign _ | Cfg_guard _ | Cfg_jump _ | Cfg_call _ -> []

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ b s =
        let evals = sign_eval s b in
        List.fold_left (fun m (i,eval) -> L.set m i eval) s evals

      let initial_state = Set.fold (fun x map ->
        Map.add x Sign_lattice.Bottom map)
        declaredVars Map.empty
    end

    include Dataflow.Forward.Make_solution(L)(Cfg)(F)(P)
  end
end
