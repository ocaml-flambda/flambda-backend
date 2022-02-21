[@@@ocaml.warning "+a-4-30-40-41-42"]

module Domain : Cfg_dataflow.Backward_domain with type t = Reg.Set.t

module Transfer : Cfg_dataflow.Backward_transfer with type domain = Domain.t

module Liveness : Cfg_dataflow.Backward_S with type domain = Domain.t
