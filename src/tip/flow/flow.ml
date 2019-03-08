open Batteries

type program = Ast.program

type func = Ast.func

type expr = Ast.expr

let expr_to_string = Printer.expr_to_string

let rec stmt_to_node =
  let open Ast in
  let open Softcheck.Softlang in
  function
    Sassign (_, lv, rv) -> create (Cfg_assign (lv, rv))
  | Soutput (_, e) -> create (Cfg_call (Eident "output", [e]))
  | Sif (_, e, b) -> create (Cfg_if (e, stmt_to_node b))
  | Sifelse (_, e, ib, eb) -> create (Cfg_if_else (e, stmt_to_node ib, stmt_to_node eb))
  | Swhile (_, e, b) -> create (Cfg_while (e, stmt_to_node b))
  | Sblock (h :: t) -> List.fold_left (fun acc s -> create (Cfg_seq (acc, stmt_to_node s))) (stmt_to_node h) t
  | Sblock [] -> assert false

let global_decls _ = []

let funcs =
  let open Softcheck.Softlang in
  let process_decls = function
      [] -> assert false
    | h :: t ->
        List.fold_left (fun acc d ->
          create (Cfg_seq (acc, create (Cfg_var_decl d)))) (create (Cfg_var_decl h)) t in
  List.map (fun f ->
    let decls = process_decls f.Ast.func_vars in
    f.Ast.func_id, f.Ast.func_vars, create (Cfg_seq (decls, stmt_to_node f.Ast.func_body)))
