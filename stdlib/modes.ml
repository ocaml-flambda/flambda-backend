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
  type 'a t = { portable : 'a @@ portable } [@@unboxed]
end

module Contended = struct
  type 'a t = { contended : 'a @@ contended } [@@unboxed]
end

module Portended = struct
  type 'a t = { portended : 'a @@ portable contended } [@@unboxed]
end

module Aliased = struct
  type 'a t = { aliased : 'a @@ aliased } [@@unboxed]
end

module Shared = struct
  type 'a t = { shared : 'a @@ shared } [@@unboxed]
end
