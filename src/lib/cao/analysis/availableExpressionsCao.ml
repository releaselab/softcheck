open Batteries
open Set.Infix
open Utils

module Ast = Cao.Ast
module Printer = Cao.Printer
module Cfg = Cao.Cfg.Cfg
module Flow = Cao.Flow

include AvailableExpressions.Make(Ast)(Printer)(Cfg)(struct
    let aexp_star p =
      let graph = Cfg.generate_from_program p in
      let blocks = Cfg.get_blocks graph in
      Hashtbl.fold (fun _ b acc -> AexpCao.aexp_block b ||. acc) blocks Set.empty

    let kill aexp_star = let open Flow in function
        VDecl _
      | CDecl _
      | FCallS _
      | Ret _
      | Ite _
      | Seq _
      | While _ -> Set.empty
      | Sample s -> List.fold_left (fun acc x -> Set.filter (Ast.contains_lv x) acc) aexp_star s
      | Assign a -> List.fold_left (fun acc x -> Set.filter (Ast.contains_lv x) acc) aexp_star a.Ast.assign_ids

    let gen s = let open Flow in match s with
      VDecl _
    | CDecl _
    | FCallS _
    | Ret _
    | Ite _
    | Seq _
    | While _ -> AexpCao.aexp_block s
    | Sample smp -> List.fold_left (fun acc x -> Set.filter (not % Ast.contains_lv x) acc) (AexpCao.aexp_block s) smp
    | Assign a -> List.fold_left (fun acc x -> Set.filter (not % Ast.contains_lv x) acc) (AexpCao.aexp_block s) a.Ast.assign_ids
  end)
