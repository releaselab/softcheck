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

module Make (Expr : Expr.S) = struct
  type expr = Expr.t

  module T = struct
    type stmt =
      | Cfg_var_decl of string
      | Cfg_assign of Expr.t * Expr.t
      | Cfg_guard of Expr.t
      | Cfg_jump
      | Cfg_call of Expr.t * Expr.t list

    and t = { stmt_label : Label.t; stmt_s : stmt } [@@deriving sexp]

    let to_string s =
      let aux = function
        | Cfg_var_decl v -> [%string "var %{v}"]
        | Cfg_assign (lv, rv) -> [%string "%{lv#Expr} := %{rv#Expr}"]
        | Cfg_guard e -> [%string "test %{e#Expr}"]
        | Cfg_jump -> "jump"
        | Cfg_call (f, args) ->
          Expr.to_string f ^ " " ^ "("
          ^ List.fold_left args
              ~f:(fun acc e -> acc ^ "," ^ Expr.to_string e)
              ~init:""
          ^ ")"
      in
      [%string "%{aux s.stmt_s} ^ %{s.stmt_label#Label}"]

    let compare s_1 s_2 = Label.compare s_1.stmt_label s_2.stmt_label
  end

  include T
  include Comparable.Make (T)
end
