open Batteries
open Set.Infix
open Utils

module Ast = Tip.Ast
module Printer = Tip.Printer
module Cfg = Tip.Cfg
module Flow = Tip.Flow

include VeryBusyExpressions.Make(Ast)(Printer)(Cfg)(struct
    let rec aexpr_rec acc = function
        Ast.Ebinop (op, e1, e2) as e ->
          (match op with
             Ast.Beq | Ast.Bgt -> aexpr_rec (aexpr_rec acc e1) e2
           | Ast.Badd | Ast.Bdiv | Ast.Bmul | Ast.Bsub -> acc <-- e)
      | Ast.Ecallf _ | Ast.Ecallfptr _ | Ast.Eident _ | Ast.Eunop _ | Ast.Ecst _
      | Ast.Enull | Ast.Einput | Ast.Emalloc  -> acc

    let aexp = aexpr_rec Set.empty

    let aexp_star p =
      let graph = Cfg.generate_from_program p in
      let blocks = Cfg.get_blocks graph in
      Hashtbl.fold (fun _ b acc -> AexpTip.aexp b ||. acc) blocks Set.empty

    let kill aexp_star = let open Flow in function
        Sassign (lv,_) -> Set.filter (Ast.contains_ident lv) aexp_star
      | Sif _ | Soutput _ | Swhile _ -> Set.empty

    let gen = let open Flow in function
        Sassign (_,rv)    -> aexp rv
      | Sif e | Swhile e  -> aexp e
      | Soutput _         -> Set.empty
  end)
