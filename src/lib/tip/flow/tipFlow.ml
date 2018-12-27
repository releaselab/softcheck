open Batteries
open Utils

module Ast = TipAst
module Printer = TipPrinter

type ast_stmt = Ast.stmt

type label = Ast.label

type program = Ast.program

type func = Ast.func

type ident = Ast.ident

type expr = Ast.expr

type block =
    Sassign of expr * expr
  | Sif     of expr
  | Soutput of expr
  | Swhile  of expr

let to_string = let open Printf in function
    Sassign(lv,rv)  -> sprintf "%s = %s" (Printer.exp_to_string lv) (Printer.exp_to_string rv)
  | Soutput(e)      -> sprintf "output %s" (Printer.exp_to_string e)
  | Sif(e)          -> sprintf "if %s" (Printer.exp_to_string e)
  | Swhile(e)       -> sprintf "while %s" (Printer.exp_to_string e)

let rec init = function
    Ast.Sassign(l,_,_)
  | Ast.Soutput(l,_)
  | Ast.Sif(l,_,_)
  | Ast.Sifelse(l,_,_,_)
  | Ast.Swhile(l,_,_)     -> l
  | Ast.Sblock b          ->
      match b with [] -> assert false | h :: _ -> init h

let final = let open Set.Infix in
  let rec final_rec acc = function
      Ast.Sassign(l,_,_)
    | Ast.Soutput(l,_)
    | Ast.Swhile(l,_,_)       -> acc <-- l
    | Ast.Sif(_,_,s)          -> final_rec acc s
    | Ast.Sifelse(_,_,si,se)  -> final_rec (final_rec acc si) se
    | Ast.Sblock b            -> match b with [] -> assert false | _ -> List.last b |> final_rec acc
  in final_rec Set.empty

let blocks f = let open Set.Infix in
  let rec blocks_rec acc = function
      Ast.Sassign(l,lv,rv)    -> acc <-- (l,Sassign(lv,rv))
    | Ast.Soutput(l,e)        -> acc <-- (l,Soutput(e))
    | Ast.Sblock b            -> List.fold_left blocks_rec acc b
    | Ast.Swhile(l,e,s)       -> blocks_rec (acc <-- (l,Swhile(e))) s
    | Ast.Sif(l,e,s)          -> blocks_rec (acc <-- (l,Sif(e))) s
    | Ast.Sifelse(l,e,si,se)  -> blocks_rec (blocks_rec (acc <-- (l,Sif(e))) si) se
  in blocks_rec Set.empty f.Ast.func_body

let labels = Set.map fst % blocks

let flow f = let open Set.Infix in let open Ast in
  let rec flow_rec acc = function
      Sassign _
    | Soutput _           -> acc
    | Sblock b                -> flow_block acc b
    | Ast.Swhile(l,_,s)       -> flow_rec (Set.map (rev_pair l) (final s) <-- (l,init s) ||. acc) s
    | Ast.Sif(l,_,s)          -> flow_rec (acc <-- (l,init s)) s
    | Ast.Sifelse(l,_,si,se)  -> flow_rec (flow_rec (acc <-- (l,init si) <-- (l,init se)) si) se
  and flow_block acc = function
      []            -> assert false
    | [s]           -> flow_rec acc s
    | s1 :: s2 :: b -> let init_s2 = init s2 in
        let final_s1 = final s1 in
        flow_block (flow_rec (flow_rec (acc ||. Set.map (rev_pair init_s2) final_s1) s1) s2) (s2 :: b)

  in flow_rec Set.empty f.func_body

let flowR = Set.map (fun (a,b) -> (b,a)) % flow