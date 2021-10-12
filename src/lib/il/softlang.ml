open Base

module type S = sig
  type expr

  module Stmt : Stmt.S with type expr = expr

  type func = string * string list * Stmt.t

  type program = string list * func list
end

module Make (E : Expr.S) = struct
  type expr = E.t [@@deriving sexp]

  module Stmt = Stmt.Make (E)

  type stmt = Stmt.t

  type func = string * string list * stmt

  type program = string list * func list
end
