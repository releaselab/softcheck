(**
Exceptions module.

All CAO exceptions should be defined here. This contemplates both
typechecking exceptions, C extraction exceptions and EasyCrypt
generations exceptions.
*)

(* ------------------------------------------------------------------ *)
exception LexicalError of string

val pp_lex_error : Format.formatter -> string -> unit

val lex_error : Lexing.lexbuf -> string -> 'a

val unterminated_comment : unit -> 'a

val unterminated_string : unit -> 'a

(* ------------------------------------------------------------------ *)
exception Invalid_expr of string
(** Exception raised when CAO is not able to convert from a CAO syntactic integer expression to a logical integer expression *)

val invalid_expr_error : string -> 'a

(* ------------------------------------------------------------------ *)
exception Unbound_value of string
(** Exception raised when an non-defined identifier is used *)

val unbound_val_error : string -> 'a

(* ------------------------------------------------------------------ *)
exception Multiple_definitions of string
(** Exception raised when an non-defined identifier is used *)

val multiple_defs_error : string -> 'a
