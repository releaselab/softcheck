open Batteries
open Softcheck

module Make(Ast : Sig.Ast)(Cfg : Sig.Flow_graph with type program = Ast.program)
    (S : sig
       val gen : Cfg.vertex -> string Set.t
       val kill : Cfg.vertex -> string Set.t
     end) = struct
  module Solve(P : sig val p : Ast.program end) = struct
    module L = Lattices.Powerset_lattice(struct
        type t = string
        let to_string = identity
      end)

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ _ b s = let open Set.Infix in
        let g = S.gen b in
        let k = S.kill b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Dataflow.Backward.Make_solution(L)(Cfg)(F)(P)
  end
end
