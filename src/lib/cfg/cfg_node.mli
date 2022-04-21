open! Core
open Scil

module type S = sig
  type expr

  type stmt =
    | Cfg_var_decl of string
    | Cfg_var_assign of string * expr
    | Cfg_assign of expr * expr
    | Cfg_guard of expr
    | Cfg_call of expr * expr list
    | Cfg_call_assign of expr * expr * expr list
    | Cfg_call_var_assign of string * expr * expr list
    | Cfg_return of expr

  and t = { stmt_label : Label.t; stmt_s : stmt }

  include Comparable.S with type t := t

  val to_string : t -> string
end

module Make : functor (Expr : Expr.S) -> S with type expr = Expr.t
