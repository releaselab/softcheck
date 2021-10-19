module Forward = struct
  module Make_solution
    (L : S.Lattice)
    (Cfg : Cfg.Flow_graph.FlowGraph)
    (F : S.Transfer with type vertex = Cfg.Vertex.t and type state = L.t)
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
    (L : S.Lattice)
    (Cfg : Cfg.Flow_graph.FlowGraph)
    (F : S.Transfer with type vertex = Cfg.Vertex.t and type state = L.t)
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
