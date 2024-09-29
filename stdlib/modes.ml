(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Thomas Del Vecchio, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Global = struct
  type 'a t = { global : 'a @@ global } [@@unboxed]
end

module Portable = struct
  type 'a t : value mod portable = { portable : 'a @@ portable } [@@unboxed]
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]
end

module Contended = struct
  type 'a t : value mod contended = { contended : 'a @@ contended } [@@unboxed]
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]
end

module Portended = struct
  type 'a t : value mod portable contended = { portended : 'a @@ portable contended }
  [@@unboxed]
  [@@unsafe_allow_any_mode_crossing "CR with-kinds"]
end
