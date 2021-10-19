open Base

module type S = sig
  type expr

  module Stmt : Stmt.S with type expr = expr

  type func = string * string list * Stmt.t

  type program = string list * func list
end

module Make : functor (Expr : Expr.S) -> S with type expr = Expr.t
