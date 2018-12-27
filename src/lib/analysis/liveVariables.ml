open Batteries
open Utils

module Make(Ast : Sig.Ast)(Cfg : Sig.Flow_graph with type stmt_label = Ast.label and type program = Ast.program)
    (S : sig
       val gen : Cfg.vertex -> Ast.ident Set.t
       val kill : Cfg.vertex -> Ast.ident Set.t
     end) = struct
  module Solve(P : sig val p : Ast.program end) = struct
    module L = Lattices.Powerset_lattice(struct
        type t = Ast.ident
        let to_string = identity
      end)

    module F = struct
      type label = Cfg.stmt_label
      type vertex = Cfg.vertex
      type state = L.property
      type ident = Cfg.ident

      let f _ _ b s = let open Set.Infix in
        let g = S.gen b in
        let k = S.kill b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Analysis.Backward.Make_solution(L)(Cfg)(F)(P)
  end
end
