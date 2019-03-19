include Softcheck.Flow_graph.Make_cfg(Sl_node)(Node)(Flow)(struct
    open Sl_node
    type func = string * string list * stmt t
    type program = Ast.program

    let funcs = Convert_to_sl.funcs
    let global_decls = Convert_to_sl.global_decls
  end)
