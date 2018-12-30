open Batteries
open Set.Infix

let rec get_non_trivial_subexpressions acc = function
    Ast.Ebinop (op, e1, e2) as e -> (match op with
      Ast.Beq | Ast.Bgt ->
        get_non_trivial_subexpressions (get_non_trivial_subexpressions acc e1) e2
    | Ast.Badd | Ast.Bdiv | Ast.Bmul | Ast.Bsub -> acc <-- e)
  | Ast.Ecallf _ | Ast.Ecallfptr _ | Ast.Eident _ | Ast.Eunop _ | Ast.Ecst _
  | Ast.Einput | Ast.Emalloc | Ast.Enull -> acc

let aexp = let open Flow in function
    Sassign (_,rv) -> get_non_trivial_subexpressions Set.empty rv
  | Sif e | Soutput e | Swhile e ->
      get_non_trivial_subexpressions Set.empty e
