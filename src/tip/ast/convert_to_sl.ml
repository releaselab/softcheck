open Batteries

type program = Ast.program

type func = Ast.func

type expr = Ast.expr

let expr_to_string = Printer.expr_to_string

let rec stmt_to_node =
  let open Ast in
  let open Sl_node in
  function
    Sassign (_, lv, rv) ->
      let lv' = create (Expr lv) in
      let rv' = create (Expr rv) in
      create (Stmt (Cfg_assign (lv', rv')))
  | Soutput (_, e) ->
      let f = create (Expr (Eident "output")) in
      let e' = create (Expr e) in
      create (Stmt (Cfg_call (f, [e'])))
  | Sif (_, e, b) ->
      let e' = create (Expr e) in
      create (Stmt (Cfg_if (e', stmt_to_node b)))
  | Sifelse (_, e, ib, eb) ->
      let e' = create (Expr e) in
      create (Stmt (Cfg_if_else (e', stmt_to_node ib, stmt_to_node eb)))
  | Swhile (_, e, b) ->
      let e' = create (Expr e) in
      create (Stmt (Cfg_while (e', stmt_to_node b)))
  | Sblock (h :: t) ->
      List.fold_left (fun acc s ->
        create (Stmt (Cfg_seq (acc, stmt_to_node s)))) (stmt_to_node h) t
  | Sblock [] -> assert false

let global_decls _ = []

let funcs =
  let open Sl_node in
  let process_decls = function
      [] -> assert false
    | h :: t ->
        List.fold_left (fun acc d ->
          create (Stmt (Cfg_seq (acc, create (Stmt (Cfg_var_decl d))))))
          (create (Stmt (Cfg_var_decl h))) t in
  List.map (fun f ->
    let decls = process_decls (List.map (fun v ->
      create (Decl v)) f.Ast.func_vars) in
    f.Ast.func_id, f.Ast.func_vars,
    create (Stmt (Cfg_seq (decls, stmt_to_node f.Ast.func_body))))
