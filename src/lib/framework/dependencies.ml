open Cfg

module Common (Cfg : Sig.FlowGraph) = struct
  type g_t = Cfg.t
end

module Forward (Cfg : Sig.FlowGraph) = struct
  include Common (Cfg)

  let outdep = Cfg.outflow

  let indep = Cfg.inflow

  let is_extremal = Cfg.is_extremal
end

module Backward (Cfg : Sig.FlowGraph) = struct
  include Common (Cfg)

  let outdep = Cfg.inflow

  let indep = Cfg.outflow

  let is_extremal = Cfg.is_extremalR
end

module InterForward
  (Cfg : Sig.FlowGraph) (M : sig
    val is_after_call : int -> bool
  end) =
struct
  include Common (Cfg)

  let outdep = Cfg.outflow

  let indep g l = if M.is_after_call l then [] else Cfg.inflow g l

  let is_extremal = Cfg.is_extremal
end

module ContextSensitiveInterForward
  (Cfg : Sig.FlowGraph) (M : sig
    val is_call_or_exit : int -> bool
  end) =
struct
  include Common (Cfg)

  let outdep g l = if M.is_call_or_exit l then [] else Cfg.outflow g l

  let indep _ _ = assert false

  let is_extremal = Cfg.is_extremal
end

module type S = sig
  type g_t

  val outdep : g_t -> int -> int list

  val indep : g_t -> int -> int list

  val is_extremal : g_t -> int -> bool
end
