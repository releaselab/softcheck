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

module Make (Expr : Expr.S) = struct
  type expr = Expr.t [@@deriving sexp]

  module T = struct
    type stmt =
      | Scil_var_decl of string
      | Scil_assign of expr * expr
      | Scil_if of expr * t
      | Scil_if_else of expr * t * t
      | Scil_while of expr * t
      | Scil_jump of t
      | Scil_call of expr * expr list
      | Scil_seq of t * t

    and t = { stmt_label : Label.t; stmt_s : stmt } [@@deriving sexp]

    let compare s_1 s_2 = Label.compare s_1.stmt_label s_2.stmt_label
  end

  include T
  include Comparable.Make (T)

  let rec to_string s =
    match s.stmt_s with
    | Scil_var_decl d -> [%string "var %{d}"]
    | Scil_assign (lv, rv) -> [%string "%{lv#Expr} := %{rv#Expr}"]
    | Scil_if (c, b) -> [%string "if %{c#Expr} then %{to_string b}"]
    | Scil_if_else (c, b_t, b_f) ->
      [%string "if %{c#Expr} then %{to_string b_t} else %{to_string b_f}"]
    | Scil_while (c, b) -> [%string "while %{c#Expr} do %{to_string b}"]
    | Scil_jump _ -> "jump"
    | Scil_call (f, args) ->
      Expr.to_string f ^ " " ^ "("
      ^ List.fold_left args
          ~f:(fun acc e -> acc ^ "," ^ Expr.to_string e)
          ~init:""
      ^ ")"
    | Scil_seq (s_1, s_2) -> [%string "%{to_string s_1}; %{to_string s_2}"]
end
