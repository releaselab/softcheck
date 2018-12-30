open Batteries
open Set.Infix

module S = struct
  type ident = Cfg.ident
  type stmt_label = Cfg.stmt_label
  type vertex = Cfg.vertex
  type blocks = (Cfg.stmt_label, Cfg.vertex) Hashtbl.t
  type definition_location = Cfg.ident * Cfg.stmt_label option

  let pair x y = (x, y)

  let find_assignments blks x =
    let open Flow in
    let aux l b acc = match b with
        Sassign (lv,_) when lv = x  -> acc ||. Set.singleton l
      | Sassign _ | Sif _ | Soutput _ | Swhile _ -> acc
    in Hashtbl.fold aux blks Set.empty

  let free_variables blocks =
    let rec free_variables_rec acc = let open Ast in function
        Ecallf (_,params)
      | Ecallfptr (_,params)  -> List.fold_left free_variables_rec acc params
      | Eident x              -> acc <-- x
      | Ebinop (_,e1,e2)      -> free_variables_rec (free_variables_rec acc e1) e2
      | Eunop (_,e)           -> free_variables_rec acc e
      | Einput | Emalloc | Enull | Ecst _ -> acc
    in let aux acc = let open Flow in function
        Sassign (_,e)
      | Sif e
      | Soutput e
      | Swhile e      -> free_variables_rec acc e
    in Hashtbl.fold (fun _ b acc -> aux acc b) blocks Set.empty

  let kill blks = let open Flow in function
      Sassign (lv,_)  -> begin let open Ast in match lv with
      Eident x  -> Set.map (pair x % Option.some) (find_assignments blks lv) <-- (x,None)
    | Ebinop _ | Ecallf _ | Ecallfptr _ | Ecst _ | Einput | Emalloc | Enull | Eunop _ -> Set.empty
    end
    | Sif _ | Soutput _ | Swhile _ -> Set.empty

  let gen l b = let open Flow in match b with
    Sassign (lv,_)  -> begin let open Ast in match lv with
    Eident x  -> Set.singleton (x,Some l)
  | Ebinop _ | Ecallf _ | Ecallfptr _ | Ecst _ | Einput | Emalloc | Enull | Eunop _ -> Set.empty
  end
  | Sif _ | Soutput _ | Swhile _ -> Set.empty
end

include Analysis.Reaching_definitions.Make(Ast)(Cfg)(S)

