open! Core
open Scil

module type S = sig
  type expr
  type program

  module Ast_block : Stmt.S with type expr = expr
  module Cfg_block : Cfg_node.S with type expr = expr
  module Edge : Comparable.S with type t = Ast_block.t * Ast_block.t

  type t = {
    correspondence : Cfg_block.t Map.M(Ast_block).t;
    blocks : Set.M(Ast_block).t;
    initial : Set.M(Ast_block).t;
    final : Set.M(Ast_block).t;
    flow : Set.M(Edge).t;
  }

  val init : Ast_block.t -> Ast_block.t
  val final : Ast_block.t -> Set.M(Ast_block).t
  val flow : Ast_block.t -> t
  val flowR : Ast_block.t -> t
end

module Make : functor
  (E : Expr.S)
  (S : Softlang.S with type expr = E.t)
  (C : Cfg_node.S with type expr = E.t)
  ->
  S
    with type expr = E.t
     and type program = S.program
     and module Ast_block = S.Stmt
     and module Cfg_block = C
