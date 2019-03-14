open Batteries
open Softcheck

module Make(Ast : Sig.Ast)
    (Cfg : Sig.Flow_graph with type program = Ast.program)
    (S : sig
       val declaredVars : Ast.program -> string Set.t
       val sign_eval : (string, Sign_lattice.property) Map.t -> Cfg.vertex ->
         (string * Sign_lattice.property) list
     end) = struct
  module Solve(P : sig val p : Ast.program end) = struct
    let declaredVars = S.declaredVars P.p

    module L = Lattices.Map_lattice(struct
        type t = string
        let to_string = identity
        let bottom_elems = declaredVars
      end)(Sign_lattice)

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ b s =
        let evals = S.sign_eval s b in
        List.fold_left (fun m (i,eval) -> L.set m i eval) s evals

      let initial_state = Set.fold (fun x map ->
        Map.add x Sign_lattice.Bottom map)
        declaredVars Map.empty
    end

    include Dataflow.Forward.Make_solution(L)(Cfg)(F)(P)
  end
end
