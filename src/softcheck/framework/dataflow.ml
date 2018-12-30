open Batteries

module Forward = struct
  module Make_solution(L : Sig.Lattice)(Cfg : Sig.Flow_graph)
      (F : Sig.Transfer with type vertex = Cfg.vertex and
      type state = L.property)
      (P : sig val p : Cfg.program end) = struct
    module Solver = Solvers.Make_fix(L)(Cfg)(F)(Dependencies.Forward(Cfg))

    let solution = Cfg.generate_from_program P.p |> Solver.solve
    let result_to_string = L.to_string
    let get_entry_result l = solution (Solver.Circ l)
    let get_exit_result l = solution (Solver.Bullet l)
  end
end

module Backward = struct
  module Make_solution(L : Sig.Lattice)(Cfg : Sig.Flow_graph)
      (F : Sig.Transfer with type vertex = Cfg.vertex and
      type state = L.property)
      (P : sig val p : Cfg.program end) =
  struct
    module Solver = Solvers.Make_fix(L)(Cfg)(F)(Dependencies.Backward(Cfg))

    let solution = Cfg.generate_from_program P.p |> Solver.solve
    let get_entry_result l = solution (Solver.Bullet l)
    let get_exit_result l = solution (Solver.Circ l)
    let result_to_string = L.to_string
  end
end

module FlowSensitiveAnalysis
    (D : sig
       type t = int val bottom_elems : t Set.t
       val to_string : t -> string
     end)
    (L : Sig.Lattice)(CfgM : Sig.Flow_graph with type stmt_label = D.t)
    (Cfg : sig val instance : CfgM.t end) = struct
  module Lattice = Lattices.Map_lattice(D)(L)
  let domain = CfgM.get_blocks Cfg.instance
end
