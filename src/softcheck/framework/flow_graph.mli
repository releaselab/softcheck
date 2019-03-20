module Make_cfg : functor
  (Sl : Softlang.S)
  (N : Node_sig.S with type expr = Sl.expr)
  (F : Sig.Flow with type block = Sl.stmt Sl.t and type vertex = N.stmt N.t)
  (C : sig
     type func = Sl.ident * Sl.ident list * Sl.stmt Sl.t
     type program
     val funcs : program -> func list
     val global_decls : program -> N.stmt N.t list
   end) -> Sig.Flow_graph with type vertex = N.stmt N.t and type expr = N.expr
                                                        and type program = C.program

module Make_inter_cfg : functor
  (Sl : Softlang.S)
  (N : Node_sig.S with type expr = Sl.expr)
  (F : Sig.Flow with type block = Sl.stmt Sl.t and type vertex = N.stmt N.t)
  (C : sig
     type program
   end) ->
  Sig.Inter_flow_graph with type vertex = N.stmt N.t and type expr = N.expr
