open Base
open Scil

module type S = sig
  type expr

  type stmt =
    | Cfg_var_decl of string
    | Cfg_assign of expr * expr
    | Cfg_guard of expr
    | Cfg_jump
    | Cfg_call of expr * expr list

  and t = { stmt_label : Label.t; stmt_s : stmt }

  include Comparable.S with type t := t

  val to_string : t -> string
end

module Make : functor (Expr : Expr.S) -> S with type expr = Expr.t
