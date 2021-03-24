open Utils

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

module Make (E : Sig.Expr) = struct
  type label = Label.t

  type decl = Decl.t

  module Expr = E

  type expr = Expr.t

  module Stmt = struct
    module T = struct
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

      let equal s_1 s_2 = Label.equal s_1.stmt_label s_2.stmt_label

      let compare s_1 s_2 = Label.compare s_1.stmt_label s_2.stmt_label

      let rec to_string s =
        match s.stmt_s with
        | Scil_var_decl d -> [%string "var %{d#Decl}"]
        | Scil_assign (lv, rv) -> [%string "%{lv#Expr} := %{rv#Expr}"]
        | Scil_if (c, b) -> [%string "if %{c#Expr} then %{to_string b}"]
        | Scil_if_else (c, b_t, b_f) ->
          [%string "if %{c#Expr} then %{to_string b_t} else %{to_string b_f}"]
        | Scil_while (c, b) -> [%string "while %{c#Expr} do %{to_string b}"]
        | Scil_jump _ -> "jump"
        | Scil_call (f, args) ->
          [%string
            "%{f#Expr} %{List.to_string ~fst:\"(\" ~sep:\", \" ~lst:\")\" \
             Expr.to_string args}"]
        | Scil_seq (s_1, s_2) -> [%string "%{to_string s_1}; %{to_string s_2}"]
    end

    include T
    include Utils.Collections.Make (T)
  end

  type stmt = Stmt.t

  type func = string * decl list * stmt

  type program = decl list * func list
end
