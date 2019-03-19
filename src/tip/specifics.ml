open Ast
open Batteries

let free_variables =
  let open Set.Infix in
  let rec free_variables_rec acc = function
      Ecallf (_,params)
    | Ecallfptr (_,params)  -> List.fold_left free_variables_rec acc params
    | Eident x              -> acc <-- x
    | Ebinop (_,e1,e2)      -> free_variables_rec (free_variables_rec acc e1) e2
    | Eunop (_,e)           -> free_variables_rec acc e
    | Einput | Emalloc | Enull | Ecst _ -> acc
  in free_variables_rec Set.empty

let is_ident = function
    Eident _ -> true
  | _ -> false

let ident_of_expr = function
    Eident x -> x
  | _ -> raise (Invalid_argument "not and ident")

let aexp =
  let open Set.Infix in
  let rec aexpr_rec acc = function
      Ebinop (op, e1, e2) as e ->
        (match op with
           Beq | Bgt -> aexpr_rec (aexpr_rec acc e1) e2
         | Badd | Bdiv | Bmul | Bsub -> acc <-- e)
    | Ecallf _ | Ecallfptr _ | Eident _ | Eunop _ | Ecst _
    | Enull | Einput | Emalloc  -> acc in
  aexpr_rec Set.empty

let rec contains_ident x = function
    Ebinop (_,e1,e2)    -> contains_ident x e1 || contains_ident x e2
  | Ecallf (_,vars)
  | Ecallfptr (_,vars)  -> List.exists (contains_ident x) vars
  | Eident _ as i       -> i = x
  | Eunop (_,e)         -> contains_ident x e
  | Ecst _ | Enull | Einput | Emalloc -> false
