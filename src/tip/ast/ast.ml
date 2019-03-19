type label = int

type loc = int * (int * int)

type unop = Uref | Uderef

type binop = Badd | Bsub | Bmul | Bdiv | Beq | Bgt

type ident = string

type constant = int

type expr =
    Ecallf of ident * expr list
  | Ecallfptr of expr * expr list
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Ecst of constant
  | Einput
  | Emalloc
  | Enull

type unlabeled_stmt =
    SUassign of  expr * expr
  | SUblock of     unlabeled_stmt list
  | SUif of      expr * unlabeled_stmt
  | SUifelse of  expr * unlabeled_stmt * unlabeled_stmt
  | SUoutput of  expr
  | SUwhile of   expr * unlabeled_stmt

type stmt =
    Sassign of  label * expr * expr
  | Sblock of   stmt list
  | Sif of      label * expr * stmt
  | Sifelse of  label * expr * stmt * stmt
  | Soutput of  label * expr
  | Swhile of   label * expr * stmt

and block = stmt list

type s_stmt =
    SSassign of   label * expr * expr
  | SSblock of    s_stmt list
  | SSif of       label * expr * s_stmt
  | SSifelse of   label * expr * s_stmt * s_stmt
  | SSoutput of   label * expr
  | SSwhile of    label * expr * s_stmt
  | SScassign of  label * label * ident * (ident * expr list)
  | SSreturn of   label * expr

type decls = ident list

type func = {
  func_id:      ident;
  func_args:    decls;
  func_vars:    decls;
  func_body:    stmt;
  func_return:  expr;
}

type s_func = {
  s_func_id:    ident;
  s_func_args:  decls;
  s_func_vars:  decls;
  s_func_body:  s_stmt;
  s_func_ln:    label;
  s_func_lx:    label;
}

type program = func list

type s_program = s_func * s_func list

let label_function i f =
  let label () =
    i := !i + 1;
    !i
  in let rec aux = function
      SUassign (e1,e2)    -> Sassign (label(),e1,e2)
    | SUblock b           -> Sblock (List.map aux b)
    | SUif (e,s)          -> let l = label() in Sif (l,e,aux s)
    | SUifelse (e,si,se)  -> let l = label() in Sifelse (l,e,aux si,aux se)
    | SUoutput (e)        -> Soutput (label(),e)
    | SUwhile (e,s)       -> let l = label() in Swhile (l,e,aux s)
  in aux f
