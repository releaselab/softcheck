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

module Make (Expr : Expr.S) = struct
  type expr = Expr.t [@@deriving sexp]

  module T = struct
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
    and t = { stmt_label : Label.t; stmt_s : stmt } [@@deriving sexp]

    let compare s_1 s_2 = Label.compare s_1.stmt_label s_2.stmt_label
  end

  include T
  include Comparable.Make (T)

  let rec seq_to_string = function
    | Seq_skip -> "skip"
    | Seq_cons (s, seq) -> [%string "%{to_string s}; %{seq_to_string seq}"]

  and to_string s =
    match s.stmt_s with
    | Scil_var_decl d -> [%string "var %{d}"]
    | Scil_var_assign (v, rv) -> [%string "%{v} := %{rv#Expr}"]
    | Scil_assign (lv, rv) -> [%string "%{lv#Expr} := %{rv#Expr}"]
    | Scil_if_else (c, b_t, b_f) ->
      [%string "if %{c#Expr} { %{to_string b_t} } { %{to_string b_f} }"]
    | Scil_while (c, b) -> [%string "while %{c#Expr} { %{to_string b} }"]
    | Scil_goto l -> [%string "goto %{l#Int}"]
    | Scil_label (l, s) -> [%string "label %{l#Int} : %{to_string s}"]
    | Scil_return e -> [%string "return %{e#Expr}"]
    | Scil_fun_decl (f, [], s) ->
      [%string "function %{f} () { %{to_string s} }"]
    | Scil_fun_decl (f, x :: xs, s) ->
      let args =
        List.fold_left xs ~f:(fun acc v -> [%string "%{acc}; var %{v}"]) ~init:x
      in
      [%string "function %{f} (%{args}) { %{to_string s} }"]
    | Scil_fun_call (f, []) -> [%string "%{f#Expr}()"]
    | Scil_fun_call (f, x :: xs) ->
      let args =
        List.fold_left xs
          ~f:(fun acc e -> [%string "%{acc}, %{e#Expr}"])
          ~init:(Expr.to_string x)
      in
      [%string "%{f#Expr}(%{args})"]
    | Scil_fun_call_var_assign (v, f, []) -> [%string "%{v} := %{f#Expr}()"]
    | Scil_fun_call_var_assign (v, f, x :: xs) ->
      let args =
        List.fold_left xs
          ~f:(fun acc e -> [%string "%{acc}, %{e#Expr}"])
          ~init:(Expr.to_string x)
      in
      [%string "%{v} := %{f#Expr}(%{args})"]
    | Scil_fun_call_assign (lv, f, []) -> [%string "%{lv#Expr} := %{f#Expr}()"]
    | Scil_fun_call_assign (lv, f, x :: xs) ->
      let args =
        List.fold_left xs
          ~f:(fun acc e -> [%string "%{acc}, %{e#Expr}"])
          ~init:(Expr.to_string x)
      in
      [%string "%{lv#Expr} := %{f#Expr}(%{args})"]
    | Scil_seq seq -> seq_to_string seq
end
