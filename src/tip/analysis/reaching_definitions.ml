open Batteries
open Set.Infix

module S = struct
  type ident = string
  type vertex = Cfg.vertex
  type blocks = Cfg.vertex Set.t
  type definition_location = ident * vertex option

  let pair x y = (x, y)

  let free_variables blocks =
    let aux acc = let open Flow in function
        Sassign (_,e)
      | Sif e
      | Soutput e
      | Swhile e      -> acc ||. Specifics.free_variables e
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

