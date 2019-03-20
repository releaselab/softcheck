open Batteries

module Forward = struct
  module Make_solution(L : Sig.Lattice)(Cfg : Sig.Flow_graph)
      (F : Sig.Transfer with type vertex = Cfg.vertex
                         and type state = L.property)
      (P : sig val graph : Cfg.t end) =
  struct
    module Solver = Solvers.Make_fix(Cfg)(L)(F)(Dependencies.Forward(Cfg))

    let solution = P.graph |> Solver.solve
    let result_to_string = L.to_string
    let get_entry_result l = solution (Solver.Circ l)
    let get_exit_result l = solution (Solver.Bullet l)
  end
end

module Backward = struct
  module Make_solution(L : Sig.Lattice)(Cfg : Sig.Flow_graph)
      (F : Sig.Transfer with type vertex = Cfg.vertex
                         and type state = L.property)
      (P : sig val graph : Cfg.t end) =
  struct
    module Solver = Solvers.Make_fix(Cfg)(L)(F)(Dependencies.Backward(Cfg))

    let solution = P.graph |> Solver.solve
    let get_entry_result l = solution (Solver.Bullet l)
    let get_exit_result l = solution (Solver.Circ l)
    let result_to_string = L.to_string
  end
end

module FlowSensitiveAnalysis
    (D : sig
       type t = int
       val bottom_elems : t Set.t
       val to_string : t -> string
     end)
    (L : Sig.Lattice)(CfgM : Sig.Flow_graph )
    (Cfg : sig val instance : CfgM.t end) = struct
  module Lattice = Lattices.Map_lattice(D)(L)
  let domain = CfgM.get_blocks Cfg.instance
end
