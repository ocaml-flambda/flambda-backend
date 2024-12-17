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
end

module Contended = struct
  type 'a t : value mod uncontended = { contended : 'a @@ contended } [@@unboxed]
end

module Portended = struct
  type 'a t : value mod portable uncontended = { portended : 'a @@ portable contended }
  [@@unboxed]
end
