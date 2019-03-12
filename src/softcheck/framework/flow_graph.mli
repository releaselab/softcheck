module Make_cfg :
  functor
    (C : sig
       type expr
       type program
       val funcs : program -> expr Softlang.func list
       val global_decls : program -> expr Cfg_node.t list
       val expr_to_string : expr -> string
     end) -> Sig.Flow_graph

module Make_inter_cfg :
  functor
    (C : sig type expr type program val expr_to_string : expr -> string end) ->
    Sig.Inter_flow_graph
