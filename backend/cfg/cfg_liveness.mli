[@@@ocaml.warning "+a-4-30-40-41-42"]

type domain =
  { before : Reg.Set.t;
    across : Reg.Set.t
  }

module Domain : Cfg_dataflow.Backward_domain with type t = domain

module Transfer : Cfg_dataflow.Backward_transfer with type domain = domain

module Liveness : Cfg_dataflow.Backward_S with type domain = domain
