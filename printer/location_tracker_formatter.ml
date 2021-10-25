(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let is_activated = ref false

let activate_tracking () = is_activated := true
let deactivate_tracking () = is_activated := false
let is_tracking () = !is_activated

type Format.stag += Location_mapping_tag of Location.t * string option

let with_location_mapping ?label ~loc ppf f =
  if is_tracking () then begin
    Format.pp_open_stag ppf (Location_mapping_tag (loc, label));
    let ret = f () in
    Format.pp_close_stag ppf ();
    ret
  end else f ()

module Mappings = struct
  module Item = struct
    type t = {
      source : Location.t;
      ir : Location.t;
      label : string option;
    }

    let create ~source ~ir ~label = { source; ir; label }
    let source { source; _ } = source
    let ir { ir; _ } = ir
    let label { label; _ } = label
  end
  type t = Item.t list

  let empty = []

  let append mapping t = mapping :: t

  let print ppf t =
    List.iter (fun Item.{source; ir; label} ->
        Format.fprintf ppf "%s%a -> %a\n"
          (match label with | Some s -> s ^ ": " | None -> "")
          Location.print_loc source
          Location.print_loc ir
      ) t

  let dump ~file t =
    let ch = open_out (file ^ ".map") in
    Marshal.to_channel ch t [];
    close_out ch
end

module Tracking_formatter = struct
  module Pending_mapping = struct
    type t = {
      label : string option;
      source : Location.t;
      start_in_buffer : Lexing.position;
    }

    let create ~label ~source ~start_in_buffer =
      { label; source; start_in_buffer }

    let to_mapping ~end_in_buffer { label; source; start_in_buffer } =
      let ir = Location.{ loc_start = start_in_buffer; loc_end = end_in_buffer; loc_ghost=true} in
      Mappings.Item.create ~source ~ir ~label
  end

  type t =
    { ppf : Format.formatter
    ; mutable pending_mappings : Pending_mapping.t list
    ; mutable mappings : Mappings.t
    ; mutable position : Lexing.position
    }

  let close t = assert (List.length t.pending_mappings = 0)
  let ppf t = t.ppf

  let track_output_position_of_formatter t =
    let ppf = t.ppf in
    let { Format.out_string; out_flush; out_newline; out_spaces; out_indent } =
      Format.pp_get_formatter_out_functions ppf ()
    in
    let track_newline () =
      (* When a newline is created [line] obviously increases by one, but also
         now both bol and char are the same as the new line is empty. *)
      t.position <-
        Lexing.{ t.position with pos_lnum = t.position.pos_lnum + 1
               ; pos_bol = t.position.pos_cnum
               }
    in
    let track_new_char () =
      t.position <-
        Lexing.{ t.position with
          pos_cnum = t.position.pos_cnum + 1
        }
    in
    let out_string str start_pos num_chars =
      for index = start_pos to start_pos + num_chars - 1 do
        track_new_char ();
        if str.[index] = '\n' then track_newline ()
      done;
      out_string str start_pos num_chars
    in
    let out_functions : Format.formatter_out_functions =
      { out_string
      ; out_flush
      ; (* The default [out_newline] calls [out_string] with a newline
           character, so we don't need to override [out_newline]. A similar
           situation exists for [out_spaces] and [out_indent]. *)
        out_newline
      ; out_spaces
      ; out_indent
      }
    in
    Format.pp_set_formatter_out_functions ppf out_functions
  ;;

  let intercept_location_tags_on_formatter t =
    let ppf = t.ppf in
    let stag_functions = Format.pp_get_formatter_stag_functions ppf () in
    let mark_open_stag = function
      | Location_mapping_tag (source, label) ->
        let pending =
          Pending_mapping.create ~label ~source ~start_in_buffer:t.position
        in
        t.pending_mappings <- pending :: t.pending_mappings;
        ""
      | stag -> stag_functions.mark_open_stag stag
    in
    let mark_close_stag = function
      | Location_mapping_tag _ ->
        (match t.pending_mappings with
         | [] -> Misc.fatal_error "End_pos_tag encountered without matching Start_pos_tag"
         | pending :: pending_mappings ->
           t.pending_mappings <- pending_mappings;
           let mapping =
             Pending_mapping.to_mapping
           ~end_in_buffer:t.position pending
           in
           t.mappings <- Mappings.append mapping t.mappings;
           "")
      | stag -> stag_functions.mark_close_stag stag
    in
    let stag_functions = { stag_functions with mark_open_stag; mark_close_stag } in
    Format.pp_set_tags ppf true;
    Format.pp_set_mark_tags ppf true;
    Format.pp_set_formatter_stag_functions ppf stag_functions
  ;;

  let create ~file ~ppf =
    let t =
      { ppf
      ; position = Lexing.{pos_fname=file; pos_lnum=1; pos_bol=0; pos_cnum=0}
      ; mappings = Mappings.empty
      ; pending_mappings = []
      }
    in
    track_output_position_of_formatter t;
    intercept_location_tags_on_formatter t;
    t
  ;;

  let mappings t = t.mappings
end
