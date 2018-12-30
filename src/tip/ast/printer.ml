open Batteries
open BatIO
open Ast

type expr = Ast.expr

let sprint_list ?(first = "") ?(last = "") ?(sep = "") to_string l =
  let strout = output_string () in
  let () = List.print ~first ~last ~sep write_string strout (List.map to_string l) in
  close_out strout

let unop_to_string = function
    Uref    -> "&"
  | Uderef  -> "*"

let binop_to_string = function
    Badd  -> "+"
  | Bsub  -> "-"
  | Bmul  -> "*"
  | Bdiv  -> "/"
  | Beq   -> "=="
  | Bgt   -> ">"

let rec exp_to_string = function
    Ecst(c)         -> string_of_int c
  | Eident x        -> x
  | Ebinop(o, a, b) -> Printf.sprintf "(%s %s %s)" (exp_to_string a)
                         (binop_to_string o) (exp_to_string b)
  | Eunop(o, e)     -> Printf.sprintf "%s%s" (unop_to_string o)
                         (exp_to_string e)
  | Einput          -> "input"
  | Ecallf(e,le)    -> Printf.sprintf "%s(%s)" e
                         (sprint_list ~first:"" ~last:"" ~sep:", " exp_to_string le)
  | Ecallfptr(e,le) -> Printf.sprintf "(%s)(%s)" (exp_to_string e)
                         (sprint_list ~first:"" ~last:"" ~sep:", " exp_to_string le)
  | Emalloc         -> "malloc"
  | Enull           -> "null"

let rec func_to_string = let open Printf in function
    Sassign (l,lv,rv)       -> sprintf "%d: %s = %s;" l (exp_to_string lv) (exp_to_string rv)
  | Sblock b                -> sprintf "{%s\n}" (List.fold_left (fun a s -> sprintf "%s\n%s" a (func_to_string s)) "" b)
  | Sif (l,e,ifb)           -> sprintf "%d: if (%s) {\n%s" l (exp_to_string e) (func_to_string ifb)
  | Sifelse (l,e,ifb,elseb) -> sprintf "%d: if (%s) {%s\n}\nelse {\n%s\n}" l (exp_to_string e) (func_to_string ifb) (func_to_string elseb)
  | Soutput (l,e)           -> sprintf "%d: output %s;" l (exp_to_string e)
  | Swhile (l,e,whileb)     -> sprintf "%d: while (%s) {\n%s\n}" l (exp_to_string e) (func_to_string whileb)

let rec s_func_to_string = let open Printf in function
    SSassign (l,lv,rv)            -> sprintf "%d: %s = %s;" l (exp_to_string lv) (exp_to_string rv)
  | SSblock b                     -> sprintf "{%s\n}" (List.fold_left (fun a s -> sprintf "%s\n%s" a (s_func_to_string s)) "" b)
  | SSif (l,e,ifb)                -> sprintf "%d: if (%s) {\n%s" l (exp_to_string e) (s_func_to_string ifb)
  | SSifelse (l,e,ifb,elseb)      -> sprintf "%d: if (%s) {%s\n}\nelse {\n%s\n}" l (exp_to_string e) (s_func_to_string ifb) (s_func_to_string elseb)
  | SSoutput (l,e)                -> sprintf "%d: output %s;" l (exp_to_string e)
  | SSwhile (l,e,whileb)          -> sprintf "%d: while (%s) {\n%s\n}" l (exp_to_string e) (s_func_to_string whileb)
  | SScassign (lc,lr,lv,(f,vars)) -> sprintf "%d %d: %s = %s;" lc lr lv (exp_to_string (Ecallf (f,vars)))
  | SSreturn (l,e)                -> sprintf "%d: return = %s;" l (exp_to_string e)
