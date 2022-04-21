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

module Make (Expr : Expr.S) = struct
  type expr = Expr.t [@@deriving sexp]

  module T = struct
    type stmt =
      | Cfg_var_decl of string
      | Cfg_var_assign of string * expr
      | Cfg_assign of expr * expr
      | Cfg_guard of expr
      | Cfg_call of expr * expr list
      | Cfg_call_assign of expr * expr * expr list
      | Cfg_call_var_assign of string * expr * expr list
      | Cfg_return of expr

    and t = { stmt_label : Label.t; stmt_s : stmt } [@@deriving sexp]

    let to_string s =
      let aux = function
        | Cfg_var_decl v -> [%string "var %{v}"]
        | Cfg_assign (lv, rv) -> [%string "%{lv#Expr} := %{rv#Expr}"]
        | Cfg_var_assign (v, rv) -> [%string "%{v} := %{rv#Expr}"]
        | Cfg_guard e -> [%string "test %{e#Expr}"]
        | Cfg_call (f, args) ->
          let args =
            List.fold_left args
              ~f:(fun acc e -> acc ^ "," ^ Expr.to_string e)
              ~init:""
          in
          [%string "%{f#Expr}(%{args})"]
        | Cfg_call_assign (lv, f, args) ->
          let args =
            List.fold_left args
              ~f:(fun acc e -> acc ^ "," ^ Expr.to_string e)
              ~init:""
          in
          [%string "%{lv#Expr} := %{f#Expr}(%{args})"]
        | Cfg_call_var_assign (v, f, args) ->
          let args =
            List.fold_left args
              ~f:(fun acc e -> acc ^ "," ^ Expr.to_string e)
              ~init:""
          in
          [%string "%{v} := %{f#Expr}(%{args})"]
        | Cfg_return e -> [%string "return %{e#Expr}"]
      in
      [%string "%{aux s.stmt_s} ^ %{s.stmt_label#Label}"]

    let compare s_1 s_2 = Label.compare s_1.stmt_label s_2.stmt_label
  end

  include T
  include Comparable.Make (T)
end
