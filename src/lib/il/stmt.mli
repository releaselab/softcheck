open! Core

module type S = sig
  type expr

  type stmt =
    | Scil_var_decl of string
    | Scil_var_assign of string * expr
    | Scil_assign of expr * expr
    | Scil_seq of seq
    | Scil_goto of int
    | Scil_label of int * t
    | Scil_return of expr
    | Scil_while of expr * t
    | Scil_if_else of expr * t * t
    | Scil_fun_decl of string * string list * t
    | Scil_fun_call of expr * expr list
    | Scil_fun_call_assign of expr * expr * expr list
    | Scil_fun_call_var_assign of string * expr * expr list

  and seq = Seq_skip | Seq_cons of t * seq
  and t = { stmt_label : Label.t; stmt_s : stmt }

  include Comparable.S with type t := t
  include Sexpable.S with type t := t

  val to_string : t -> string
end

module Make (Expr : Expr.S) : S with type expr = Expr.t
