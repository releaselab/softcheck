(* ------------------------------------------------------------------ *)
exception LexicalError of string

let pp_lex_error fmt msg =
  Format.fprintf fmt "parse error: %s" msg

let lex_error _ msg =
  raise (LexicalError msg)

let unterminated_comment () =
  raise (LexicalError "unterminated comment")

let unterminated_string () =
  raise (LexicalError "unterminated string")

(* ------------------------------------------------------------------ *)
exception Invalid_expr of string

let invalid_expr_error msg =
  raise (Invalid_expr msg)

(* ------------------------------------------------------------------ *)
exception Unbound_value of string

let unbound_val_error msg =
  raise (Unbound_value msg)

(* ------------------------------------------------------------------ *)
exception Multiple_definitions of string

let multiple_defs_error msg =
  raise (Unbound_value msg)

(*exception Invalid_constant of string

  exception Unable_to_eval of expr L.located

  exception Unbound_value of string

  exception Size_lower_1 of string

  exception Mod_lower_2 of string

  exception Not_prime of string

  exception Invalid_mod_type of string

  exception Multiple_declarations of string

  exception SeqVar*)
