include Softcheck.Cfg_node.Make(struct
    type t = Ast.expr
    let to_string = Printer.expr_to_string
  end)
