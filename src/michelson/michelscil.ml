type unop = Fst | Snd

type binop = Eq

type data = Ast.data

type expr =
  | E_unop of unop * expr
  | E_binop of binop * expr * expr
  | E_data of data
  | E_ident of string
  | E_cons of expr * expr

and stmt =
  | S_seq of stmt * stmt
  | S_var_decl of string
  | S_assign of string * expr
  | S_if of expr * stmt * stmt
  | S_skip
  | S_todo
