module Forward = struct
  module Make_solution
    (L : Lattice.Sig.S)
    (Cfg : Cfg.Sig.FlowGraph)
    (F : S.Transfer with type vertex = Cfg.vertex and type state = L.property)
    (P : sig
      val graph : Cfg.t
    end) =
  struct
    module Solver = Solvers.Make_fix (Cfg) (L) (F) (Dependencies.Forward (Cfg))

    let solution = P.graph |> Solver.solve

    let result_to_string = L.to_string

    let get_entry_result l = solution (Solver.Circ l)

    let get_exit_result l = solution (Solver.Bullet l)
  end
end

module Backward = struct
  module Make_solution
    (L : Lattice.Sig.S)
    (Cfg : Cfg.Sig.FlowGraph)
    (F : S.Transfer with type vertex = Cfg.vertex and type state = L.property)
    (P : sig
      val graph : Cfg.t
    end) =
  struct
    module Solver = Solvers.Make_fix (Cfg) (L) (F) (Dependencies.Backward (Cfg))

    let solution = P.graph |> Solver.solve

    let get_entry_result l = solution (Solver.Bullet l)

    let get_exit_result l = solution (Solver.Circ l)

    let result_to_string = L.to_string
  end
end

module FlowSensitiveAnalysis (D : sig
  include Utils.Collections.WithCollections

  val bottom_elems : Set.t
end)
(L : Lattice.Sig.S)
(CfgM : Cfg.Sig.FlowGraph) (Cfg : sig
  val instance : CfgM.t
end) =
struct
  module Lattice = Lattice.Map.Make (D) (L)

  let domain = CfgM.get_blocks Cfg.instance
end
