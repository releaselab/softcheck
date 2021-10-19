open Base

module type S = sig
  type expr

  type stmt =
    | Scil_var_decl of string
    | Scil_assign of expr * expr
    | Scil_if of expr * t
    | Scil_if_else of expr * t * t
    | Scil_while of expr * t
    | Scil_jump of t
    | Scil_call of expr * expr list
    | Scil_seq of t * t

  and t = { stmt_label : Label.t; stmt_s : stmt }

  include Comparable.S with type t := t

  include Sexpable.S with type t := t

  val to_string : t -> string
end

module Make (Expr : Expr.S) : S with type expr = Expr.t
