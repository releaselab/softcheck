open Batteries
open Set.Infix

include Analysis.Live_variables.Make(Ast)(Cfg)(struct
    let free_variables e =
      let open Ast in
      let rec free_variables_rec acc = function
          Ecallf (_,params)
        | Ecallfptr (_,params)  -> List.fold_left free_variables_rec acc params
        | Eident x              -> acc <-- x
        | Ebinop (_,e1,e2)      -> free_variables_rec (free_variables_rec acc e1) e2
        | Eunop (_,e)           -> free_variables_rec acc e
        | Einput | Emalloc | Enull | Ecst _ -> acc
      in free_variables_rec Set.empty e

    let kill = let open Flow in function
        Sassign (lv,_)                -> free_variables lv
      | Sif _ | Soutput _ | Swhile _  -> Set.empty

    let gen = let open Flow in function
        Sassign (_,rv)  -> free_variables rv
      | Sif c
      | Swhile c
      | Soutput c       -> free_variables c
  end)
