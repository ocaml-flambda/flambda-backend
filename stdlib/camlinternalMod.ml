# 1 "camlinternalMod.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Xavier Leroy, projet Cristal, INRIA Rocquencourt              *)
(*                                                                        *)
(*   Copyright 2004 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

<<<<<<< HEAD
open! Stdlib

[@@@ocaml.flambda_o3]

||||||| 24dbb0976a
external make_forward : Obj.t -> Obj.t -> unit = "caml_obj_make_forward"

=======
>>>>>>> ocaml/4.14
type shape =
  | Function
  | Lazy
  | Class
  | Module of shape array
  | Value of Obj.t

let rec init_mod_field modu i loc shape =
  let init =
    match shape with
    | Function ->
       let rec fn (x : 'a) =
         let fn' : 'a -> 'b = Obj.obj (Obj.field modu i) in
         if fn == fn' then
           raise (Undefined_recursive_module loc)
         else
           fn' x in
       Obj.repr fn
    | Lazy ->
       let rec l =
         lazy (
           let l' = Obj.obj (Obj.field modu i) in
           if l == l' then
             raise (Undefined_recursive_module loc)
           else
             Lazy.force l') in
       Obj.repr l
    | Class ->
       Obj.repr (CamlinternalOO.dummy_class loc)
    | Module comps ->
       Obj.repr (init_mod_block loc comps)
    | Value v -> v
  in
  Obj.set_field modu i init

and init_mod_block loc comps =
  let length = Array.length comps in
  let modu = Obj.new_block 0 length in
  for i = 0 to length - 1 do
    init_mod_field modu i loc comps.(i)
  done;
  modu

<<<<<<< HEAD
let [@inline never] init_mod loc shape =
||||||| 24dbb0976a
let overwrite o n =
  assert (Obj.size o >= Obj.size n);
  for i = 0 to Obj.size n - 1 do
    Obj.set_field o i (Obj.field n i)
=======
let init_mod loc shape =
>>>>>>> ocaml/4.14
  match shape with
  | Module comps ->
     Obj.repr (init_mod_block loc comps)
  | _ -> failwith "CamlinternalMod.init_mod: not a module"

let rec update_mod_field modu i shape n =
  match shape with
  | Function | Lazy ->
     Obj.set_field modu i n
  | Value _ ->
     () (* the value is already there *)
  | Class ->
     assert (Obj.tag n = 0 && Obj.size n = 4);
     let cl = Obj.field modu i in
     for j = 0 to 3 do
       Obj.set_field cl j (Obj.field n j)
     done
  | Module comps ->
     update_mod_block comps (Obj.field modu i) n

and update_mod_block comps o n =
  assert (Obj.tag n = 0 && Obj.size n >= Array.length comps);
  for i = 0 to Array.length comps - 1 do
    update_mod_field o i comps.(i) (Obj.field n i)
  done

<<<<<<< HEAD
let [@inline never] update_mod shape o n =
||||||| 24dbb0976a
let overwrite_closure o n =
  (* We need to use the [raw_field] functions at least on the code
     pointer, which is not a valid value in -no-naked-pointers
     mode. *)
  assert (Obj.tag n = Obj.closure_tag);
  assert (Obj.size o >= Obj.size n);
  let n_start_env = Obj.Closure.((info n).start_env) in
  let o_start_env = Obj.Closure.((info o).start_env) in
  (* if the environment of n starts before the one of o,
     clear the raw fields in between. *)
  for i = n_start_env to o_start_env - 1 do
    Obj.set_raw_field o i Nativeint.one
  done;
  (* if the environment of o starts before the one of n,
     clear the environment fields in between. *)
  for i = o_start_env to n_start_env - 1 do
    Obj.set_field o i (Obj.repr ())
  done;
  for i = 0 to n_start_env - 1 do
    (* code pointers, closure info fields, infix headers *)
    Obj.set_raw_field o i (Obj.raw_field n i)
  done;
  for i = n_start_env to Obj.size n - 1 do
    (* environment fields *)
    Obj.set_field o i (Obj.field n i)
  done;
  for i = Obj.size n to Obj.size o - 1 do
    (* clear the leftover space *)
    Obj.set_field o i (Obj.repr ())
  done;
  ()

let rec init_mod loc shape =
=======
let update_mod shape o n =
>>>>>>> ocaml/4.14
  match shape with
  | Module comps ->
     update_mod_block comps o n
  | _ -> failwith "CamlinternalMod.update_mod: not a module"
