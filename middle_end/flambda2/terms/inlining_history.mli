(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2021--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Track the history of a term through the inliner.
 *   Each apply and function declaration node is associated to an history.
 *   This history possess information about the how the term was created.
 *
 *   For example take the following piece of code:
 *   ```
 *   (* A.ml *)
 *   let foo arg =
 *      let[@inline never] bar arg = .... in
 *      bar x
 *
 *   let baz x = foo ()
 *   ```
 *   - the function [foo] is associated to the history [A.foo]
 *   - the function [bar] is associated to the history [A.foo.bar]
 *   - the call to [bar] is associated to the history [A.foo calls A.foo.bar]
 *   - the call to [foo] is associated to the history [A.baz calls A.foo]
 *
 *   Now if [foo] gets inline the code becomes:
 *   ```
 *   (* A.ml *)
 *   let baz x =
 *      let[@inline never] bar arg = .... in
 *      bar x
 *   ```
 *   - the function [baz] is associated to the history [A.baz]
 *   - the function [bar] is associated to the history [A.bar coming from inlining A.bar]
 *   - the call to [bar] is associated to the history [A.baz calls (A.bar which was coming from inlining A.bar)]
 *
 *   This modules exports three submodules:
 *   - [Absolute] which is an inlining history rooted to the toplevel a
 *     compilation unit.
 *   - [Relative] which is a chunk of an inlining history. It describes what
 *     happened since the last recorded apply/function node.
 *   - [Tracker] the tracker deals with the most common operations (inlining
 *     an apply node, declaring a new function...) while traversing terms.
 *)

module Absolute : sig
  type t

  and path =
    | Empty
    | Unknown of { prev : path }
    | Function of
        { dbg : Debuginfo.t;
          name : string;
          prev : path
        }
    | Module of
        { name : string;
          prev : path
        }
    | Class of
        { name : string;
          prev : path
        }
    | Call of
        { dbg : Debuginfo.t;
          callee : t;
          created_at : t;
          prev : path
        }
    | Inline of { prev : path }

  val empty : Compilation_unit.t -> t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val uid_path : path -> string

  val compilation_unit : t -> Compilation_unit.t

  val path : t -> path

  val to_string : t -> string

  (* Returns a shorter version of [t] that roughly corresponds to its original
     (meaning before any inlining occurred) path. *)
  val shorten_to_definition : t -> t
end

module Relative : sig
  type t

  val empty : t

  (* [concat ~earlier ~later] concatenates two relative history. In the
     resulting history [earlier] happens before [later].*)
  val concat : earlier:t -> later:t -> t

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit

  (* [between_scoped_location ~parent ~child] returns the relative path between
     [Absolute.of_scoped_location parent] and [Absolute.of_scope_location
     child] *)
  val between_scoped_locations :
    parent:Debuginfo.Scoped_location.t -> child:Debuginfo.Scoped_location.t -> t
end

module Tracker : sig
  type t

  val empty : Compilation_unit.t -> t

  val absolute : t -> Absolute.t

  val relative : t -> Relative.t

  (* To be called when defining a closure.

     Returns the absolute path and the relative path of a closure. The relative
     path does not include the [Fundecl] atom

     [function_relative_history] is the relative history stored on the closure
     previously. *)
  val fundecl :
    dbg:Debuginfo.t ->
    function_relative_history:Relative.t ->
    name:string ->
    t ->
    Absolute.t * Relative.t

  (* To be call when a call is inlined. Returns the tracker to use inside the
     inlined call. *)
  val enter_inlined_apply :
    dbg:Debuginfo.t ->
    callee:Absolute.t ->
    apply_relative_history:Relative.t ->
    t ->
    t

  (* Returns the tracker to be used while simplifying a function with the
     provided absolute history. *)
  val inside_function : Absolute.t -> t

  (* [fundecl_of_scoped_location ~path_to_root name path] constructs the
     relative history happening between [path] and [path_to_root]. This relative
     history is then ended by a function declaration. *)
  val fundecl_of_scoped_location :
    name:string ->
    path_to_root:Debuginfo.Scoped_location.t ->
    Debuginfo.Scoped_location.t ->
    t ->
    Absolute.t * Relative.t

  (* The next two functions should only be called when recording a decision made
     on a call site. *)
  val call :
    dbg:Debuginfo.t ->
    callee:Absolute.t ->
    relative:Relative.t ->
    t ->
    Absolute.t

  val unknown_call : dbg:Debuginfo.t -> relative:Relative.t -> t -> Absolute.t
end
