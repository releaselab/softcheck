module type S = sig
  type label = Label.t

  type decl = Decl.t

  module Expr : Sig.Expr

  type expr = Expr.t

  module Stmt : sig
    type stmt =
      | Scil_var_decl of decl
      | Scil_assign of expr * expr
      | Scil_if of expr * t
      | Scil_if_else of expr * t * t
      | Scil_while of expr * t
      | Scil_jump of t
      | Scil_call of expr * expr list
      | Scil_seq of t * t

    and t = { stmt_label : label; stmt_s : stmt }

    include Utils.Collections.WithCollections with type t := t
  end

  type stmt = Stmt.t

  type func = string * decl list * stmt

  type program = decl list * func list
end

module Make : functor
  (E : Sig.Expr)
  -> S with type Expr.t = E.t
