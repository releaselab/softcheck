module type S = sig
  include Scil.Softlang.S

  module Stmt : sig
    type stmt =
      | Cfg_var_decl of decl
      | Cfg_assign of expr * expr
      | Cfg_guard of expr
      | Cfg_jump
      | Cfg_call of expr * expr list

    and t = { stmt_label : label; stmt_s : stmt }

    include Utils.Collections.WithCollections with type t := t
  end

  type stmt = Stmt.t
end

module Make : functor (S : Scil.Softlang.S) -> S with type Expr.t = S.Expr.t
