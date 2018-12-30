open Batteries
open Set.Infix

include Analysis.Available_expressions.Make(Ast)(Printer)(Cfg)(struct
    let rec contains_ident x = function
        Ast.Ebinop (_,e1,e2)    -> contains_ident x e1 || contains_ident x e2
      | Ast.Ecallf (_,vars)
      | Ast.Ecallfptr (_,vars)  -> List.exists (contains_ident x) vars
      | Ast.Eident _ as i       -> i = x
      | Ast.Eunop (_,e)         -> contains_ident x e
      | Ast.Ecst _ | Ast.Einput | Ast.Emalloc | Ast.Enull -> false

    let aexp_star p =
      let graph = Cfg.generate_from_program p in
      let blocks = Cfg.get_blocks graph in
      Hashtbl.fold (fun _ b acc -> Aexp_tip.aexp b ||. acc) blocks Set.empty

    let kill aexp_star = let open Flow in function
        Sassign (lv,_)  -> Set.filter (contains_ident lv) aexp_star
      | Sif _ | Soutput _ | Swhile _ -> Set.empty

    let gen s = let open Flow in match s with
      Sassign (lv,_) -> Set.filter (not % contains_ident lv) (Aexp_tip.aexp s)
    | Sif _ | Swhile _ | Soutput _ -> Aexp_tip.aexp s
  end)
