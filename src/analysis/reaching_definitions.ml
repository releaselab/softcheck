open Batteries
open Set.Infix
open Softcheck

module type Language_component = sig
  type vertex  
  type blocks = (int, vertex) Hashtbl.t
  type definition_location = string * int option

  val free_variables : blocks -> string Set.t
  val gen : int -> vertex -> definition_location Set.t
  val kill : blocks -> vertex -> definition_location Set.t
end

module Make(Ast : Sig.Ast)
    (Cfg : Sig.Flow_graph with type program = Ast.program)
    (S : Language_component with type vertex = Cfg.vertex) = struct
  module Solve(P : sig val p : Ast.program end) = struct
    let graph = Cfg.generate_from_program P.p
    let blocks = Cfg.get_blocks graph

    module L = Lattices.Powerset_lattice(struct
        type t = S.definition_location
        let to_string (v,l) = Printf.sprintf "(%s,%s)" v (match l with
            None    -> "?"
          | Some l' -> string_of_int l')
      end)

    module F = struct
      type vertex = Cfg.vertex
      type state = L.property

      let f _ l b s =
        let g = S.gen l b in
        let k = S.kill blocks b in
        (s -. k) ||. g

      let initial_state = Set.map (fun x -> x,None) (S.free_variables blocks)
    end

    module Fix = Solvers.Make_fix(L)(Cfg)(F)(Dependencies.Forward(Cfg))

    let solution = Fix.solve graph

    let get_entry_result l = solution (Fix.Circ l)
    let get_exit_result l = solution (Fix.Bullet l)
    let result_to_string = L.to_string
  end
end
