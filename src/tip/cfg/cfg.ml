include Softcheck.Flow_graph.Make_cfg(struct
    type expr = Ast.expr
    type program = Ast.program

    let funcs = Flow.funcs
    let global_decls = Flow.global_decls
    let expr_to_string = Printer.expr_to_string
  end)
