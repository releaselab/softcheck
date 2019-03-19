open Batteries

module S = struct
  type expr = Ast.expr
  type ident = string
  type vertex = Cfg.vertex
  type blocks = Cfg.vertex Set.t
  type definition_location = ident * vertex option

  let free_variables = Specifics.free_variables

  let is_ident = Specifics.is_ident

  let ident_of_expr = Specifics.ident_of_expr
end

include Analysis.Reaching_definitions.Make(Node)(Cfg)(S)

