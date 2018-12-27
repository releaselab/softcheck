open Batteries
open Utils

module Make(Ast : Sig.Ast)
    (Cfg : Sig.Flow_graph with type ident = Ast.ident and
    type stmt_label = Ast.label and
    type program = Ast.program)
    (S : sig
       val declaredVars : Ast.program -> Ast.ident Set.t
       val sign_eval : (Cfg.ident, Sign_lattice.property) Map.t -> Cfg.vertex ->
         (Cfg.ident * Sign_lattice.property) list
     end) = struct
  module Solve(P : sig val p : Ast.program end) = struct
    let declaredVars = S.declaredVars P.p

    module L = Lattices.Map_lattice(struct
        type t = Ast.ident
        let to_string = identity
        let bottom_elems = declaredVars
      end)(Sign_lattice)

    module F = struct
      type label = Cfg.stmt_label
      type vertex = Cfg.vertex
      type state = L.property
      type ident = Cfg.ident

      let f _ _ b s =
        let evals = S.sign_eval s b in
        List.fold_left (fun m (i,eval) -> L.set m i eval) s evals

      let initial_state = Set.fold (fun x map ->
        Map.add x Sign_lattice.Bottom map)
        declaredVars Map.empty
    end

    include Analysis.Forward.Make_solution(L)(Cfg)(F)(P)
  end
end