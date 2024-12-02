(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of keyboard interrupts *)

let interrupted = Domain.DLS.new_key_safe (fun _ -> false)

let is_protected = Domain.DLS.new_key_safe (fun _ -> false)

let break _signum : unit =
  Domain.DLS.with_password (fun pw ->
    if Domain.DLS.get' pw is_protected
    then Domain.DLS.set' pw interrupted true
    else raise Sys.Break)

let _ =
  match Sys.os_type with
    "Win32" -> ()
  | _ ->
      Sys.set_signal_safe Sys.sigint (Sys.Signal_handle break);
      Sys.set_signal_safe Sys.sigpipe (Sys.Signal_handle(fun _ -> raise End_of_file))

let protect f =
  if Domain.DLS.get is_protected then
    f ()
  else begin
    Domain.DLS.set is_protected true;
    if not (Domain.DLS.get interrupted) then
       f ();
    Domain.DLS.set is_protected false;
    if Domain.DLS.get interrupted then begin
      Domain.DLS.set interrupted false;
      raise Sys.Break
    end
  end

let unprotect f =
  if not (Domain.DLS.get is_protected) then
    f ()
  else begin
    Domain.DLS.set is_protected false;
    if (Domain.DLS.get interrupted) then begin
      Domain.DLS.set interrupted false;
      raise Sys.Break
    end;
    f ();
    Domain.DLS.set is_protected true
  end
