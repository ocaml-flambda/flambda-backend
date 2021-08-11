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

module Location_in_file = struct
  type t = {
    line : int;
    char : int;
    bol : int;
  }

  let print ppf { line; char; bol } = Format.fprintf ppf "{line=%d; char=%d; bol=%d}" line char bol

  let create ~line ~char ~bol =
    { line; char; bol }

  let line { line; _ } = line
  let char { char; _ } = char
  let bol { bol; _ } = bol
end

module Range = struct
  type t = {
    file : string;
    start : Location_in_file.t;
    end_ : Location_in_file.t;
  }

  let print ppf {file; start; end_} =
    Format.fprintf ppf "{file=%s; start=%a; end=%a}" file Location_in_file.print start Location_in_file.print end_

  let create ~file ~start ~end_ = { file; end_; start }

  let of_location (location : Location.t) =
    let loc_start = location.loc_start in
    let loc_end = location.loc_end in
    { file = loc_start.pos_fname
      ; start = Location_in_file.create ~line:loc_start.pos_lnum
          ~char:loc_start.pos_cnum ~bol:loc_start.pos_bol
     ; end_ = Location_in_file.create ~line:loc_end.pos_lnum
          ~char:loc_end.pos_cnum ~bol:loc_end.pos_bol
    }

  let file { file; _ } = file
  let start { start; _ } = start
  let end_ { end_; _ } = end_
end

type Format.stag += Location_mapping_tag of Range.t * string option

let with_location_mapping ?label ~loc ppf f =
  if is_tracking () then begin
    let range = Range.of_location loc in
    Format.pp_open_stag ppf (Location_mapping_tag (range, label));
    let ret = f () in
    Format.pp_close_stag ppf ();
    ret
  end else f ()

module Mappings = struct
  module Item = struct
    type t = {
      source : Range.t;
      ir : Range.t;
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
        Format.fprintf ppf "%s: %a -> %a\n"
          (match label with | Some s -> s | None -> "")
          Range.print source
          Range.print ir
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
      source : Range.t;
      start_in_buffer : Location_in_file.t;
    }

    let create ~label ~source ~start_in_buffer =
      { label; source; start_in_buffer }

    let to_mapping ~file ~end_in_buffer { label; source; start_in_buffer } =
      let ir = Range.create ~file ~start:start_in_buffer ~end_:end_in_buffer in
      Mappings.Item.create ~source ~ir ~label
  end

  type t =
    { ppf : Format.formatter
    ; file : string
    ; mutable pending_mappings : Pending_mapping.t list
    ; mutable mappings : Mappings.t
    ; mutable position : Location_in_file.t
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
        { line = t.position.line + 1
        ; bol = t.position.char
        ; char = t.position.char
        }
    in
    let track_new_char () =
      t.position <-
        { t.position with
          char = t.position.char + 1
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
         | [] -> failwith "End_pos_tag encountered without matching Start_pos_tag"
         | pending :: pending_mappings ->
           t.pending_mappings <- pending_mappings;
           let mapping =
             Pending_mapping.to_mapping
               ~file:t.file ~end_in_buffer:t.position pending
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
      ; position = Location_in_file.create ~line:1 ~bol:0 ~char:0
      ; mappings = Mappings.empty
      ; pending_mappings = []
      ; file
      }
    in
    track_output_position_of_formatter t;
    intercept_location_tags_on_formatter t;
    t
  ;;

  let mappings t = t.mappings
end
