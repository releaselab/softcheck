open Batteries
open Set.Infix
open Softcheck

module Make(Ast : sig
    include Sig.Ast
  end)
    (Printer : Sig.Printer with type expr = Ast.expr)
    (Cfg : Sig.Flow_graph with type program = Ast.program)
    (S : sig
       val aexp_star : Ast.program -> Ast.expr Set.t
       val gen : Cfg.vertex -> Ast.expr Set.t
       val kill : Ast.expr Set.t -> Cfg.vertex -> Ast.expr Set.t
     end) = struct
  module Solve(P : sig val p : Ast.program end) = struct
    let aexp_star = S.aexp_star P.p
    module L = Lattices.Reverse_powerset_lattice(struct
        type t = Ast.expr
        let bottom = aexp_star
        let to_string = Printer.expr_to_string
      end)

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ b s =
        let g = S.gen b in
        let k = S.kill aexp_star b in
        (s -. k) ||. g

      let initial_state = Set.empty
    end

    include Dataflow.Backward.Make_solution(L)(Cfg)(F)(P)
  end
end
