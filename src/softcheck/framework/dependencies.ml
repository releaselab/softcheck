module Common(Cfg : Sig.Flow_graph) = struct
  type g_t = Cfg.t
  type vertex = Cfg.vertex
end

module Forward(Cfg : Sig.Flow_graph) = struct
  include Common(Cfg)

  let outdep      = Cfg.outflow
  let indep       = Cfg.inflow
  let is_extremal = Cfg.is_extremal
end

module Backward(Cfg : Sig.Flow_graph) = struct
  include Common(Cfg)

  let outdep      = Cfg.inflow
  let indep       = Cfg.outflow
  let is_extremal = Cfg.is_extremalR
end

module InterForward(Cfg : Sig.Flow_graph)
    (M : sig val is_after_call : Cfg.vertex -> bool end) = struct
  include Common(Cfg)

  let outdep      = Cfg.outflow
  let indep g l   = if M.is_after_call l then [] else Cfg.inflow g l
  let is_extremal = Cfg.is_extremal
end

module ContextSensitiveInterForward(Cfg : Sig.Flow_graph)
    (M : sig val is_call_or_exit : Cfg.vertex -> bool end) = struct
  include Common(Cfg)

  let outdep g l  = if M.is_call_or_exit l then [] else Cfg.outflow g l
  let indep _ _   = assert false
  let is_extremal = Cfg.is_extremal
end
