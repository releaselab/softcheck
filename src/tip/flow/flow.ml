open Batteries

type program = Ast.program

type func = Ast.func

type expr = Ast.expr

let expr_to_string = Printer.expr_to_string

let rec stmt_to_node =
  let open Ast in
  let open Softcheck.Cfg_node.Intermediate in
  function
    Sassign (_, lv, rv) -> create (Cfg_assign (lv, rv))
  | Soutput (_, e) -> create (Cfg_call (Eident "output", [e]))
  | Sif (_, e, b) -> create (Cfg_if (e, stmt_to_node b))
  | Sifelse (_, e, ib, eb) -> create (Cfg_if_else (e, stmt_to_node ib, stmt_to_node eb))
  | Swhile (_, e, b) -> create (Cfg_while (e, stmt_to_node b))
  | Sblock (h :: t) -> List.fold_left (fun acc s -> create (Cfg_seq (acc, stmt_to_node s))) (stmt_to_node h) t
  | Sblock [] -> create Cfg_skip

let global_decls _ = []

let funcs =
  let open Softcheck.Cfg_node.Intermediate in
  let process_decls = function
      [] -> create Cfg_skip
    | h :: t ->
        List.fold_left (fun acc d ->
          create (Cfg_seq (acc, create (Cfg_var_decl d)))) (create (Cfg_var_decl h)) t in
  List.map (fun f ->
    let decls = process_decls f.Ast.func_vars in
    f.Ast.func_id, f.Ast.func_vars, create (Cfg_seq (decls, stmt_to_node f.Ast.func_body)))

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

let rev_pair x y = (y, x)

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
