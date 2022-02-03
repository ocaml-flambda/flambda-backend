[@@@ocaml.warning "+a-4-30-40-41-42"]

module Domain : Cfg_dataflow.DomainXXX with type t = Reg.Set.t

module Transfer : Cfg_dataflow.TransferXXX with type domain = Domain.t

module Liveness : Cfg_dataflow.SXXX with type domain = Domain.t
