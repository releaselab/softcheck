module Make_cfg : functor
  (Sl : Scil.Softlang.S)
  (N : Cfg_node.S with type Expr.t = Sl.Expr.t)
  (F : Sig.Flow
         with type Ast_block.t = Sl.Stmt.t
          and type Cfg_block.t = N.Stmt.t)
  (C : sig
     type program

     val convert_program_to_sl : program -> Sl.program
   end)
  ->
  Sig.FlowGraph
    with type block = N.Stmt.t
     and type expr = N.Expr.t
     and type program = C.program

module Make_inter_cfg : functor
  (Sl : Scil.Softlang.S)
  (N : Cfg_node.S with type Expr.t = Sl.Expr.t)
  (F : Sig.Flow
         with type Ast_block.t = Sl.Stmt.t
          and type Cfg_block.t = N.Stmt.t)
  (C : sig
     type program
   end)
  -> Sig.InterFlowGraph with type block = N.Stmt.t and type expr = N.Expr.t
