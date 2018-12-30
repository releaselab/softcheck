module Forward(Cfg : Sig.Flow_graph) = struct
  type g_t = Cfg.t
  type label = Cfg.stmt_label

  let outdep      = Cfg.outflow
  let indep       = Cfg.inflow
  let is_extremal = Cfg.is_extremal
end

module Backward(Cfg : Sig.Flow_graph) = struct
  type g_t = Cfg.t
  type label = Cfg.stmt_label

  let outdep      = Cfg.inflow
  let indep       = Cfg.outflow
  let is_extremal = Cfg.is_extremalR
end

module InterForward(Cfg : Sig.Flow_graph)
    (M : sig val is_after_call : Cfg.stmt_label -> bool end) = struct
  type g_t = Cfg.t
  type label = Cfg.stmt_label

  let outdep      = Cfg.outflow
  let indep g l   = if M.is_after_call l then [] else Cfg.inflow g l
  let is_extremal = Cfg.is_extremal
end

module ContextSensitiveInterForward(Cfg : Sig.Flow_graph)
    (M : sig val is_call_or_exit : Cfg.stmt_label -> bool end) = struct
  type g_t = Cfg.t
  type label = Cfg.stmt_label

  let outdep g l  = if M.is_call_or_exit l then [] else Cfg.outflow g l
  let indep _ _   = assert false
  let is_extremal = Cfg.is_extremal
end
