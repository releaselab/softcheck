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

