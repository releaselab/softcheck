module Make_cfg : functor
  (Sl : Softlang.S)
  (N : Cfg_node.S with type expr = Sl.expr)
  (F : Sig.Flow with type block = Sl.stmt Sl.t and type vertex = N.stmt N.t)
  (C : sig
     type program

     val convert_program_to_sl : program -> Sl.program
   end) -> Sig.Flow_graph with type vertex = N.stmt N.t and type expr = N.expr
                                                        and type program = C.program

module Make_inter_cfg : functor
  (Sl : Softlang.S)
  (N : Cfg_node.S with type expr = Sl.expr)
  (F : Sig.Flow with type block = Sl.stmt Sl.t and type vertex = N.stmt N.t)
  (C : sig
     type program
   end) ->
  Sig.Inter_flow_graph with type vertex = N.stmt N.t and type expr = N.expr
