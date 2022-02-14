(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let colour_enabled =
  lazy
    ((* This avoids having to alter misc.ml *)
     let buf = Buffer.create 10 in
     let ppf = Format.formatter_of_buffer buf in
     Misc.Color.set_color_tag_handling ppf;
     Format.fprintf ppf "@{<error>@}%!";
     String.length (Buffer.contents buf) > 0)

let normal () = if Lazy.force colour_enabled then "\x1b[0m" else ""

let fg_256 n =
  if Lazy.force colour_enabled then Printf.sprintf "\x1b[38;5;%d;1m" n else ""

let bg_256 n =
  if Lazy.force colour_enabled then Printf.sprintf "\x1b[48;5;%d;1m" n else ""

let prim_constructive () = fg_256 163

let prim_destructive () = fg_256 62

let prim_neither () = fg_256 130

let naked_number () = fg_256 70

let tagged_immediate () = fg_256 70

let constructor () = fg_256 69

let kind () = fg_256 37

let subkind () = fg_256 39

let top_or_bottom_type () = fg_256 37

let debuginfo () = fg_256 243

let discriminant () = fg_256 111

let name () = fg_256 111

let parameter () = fg_256 198

let symbol () = fg_256 98

let variable () = fg_256 111

let closure_element () = fg_256 31

let closure_var () = fg_256 43

let code_id () = fg_256 169

let expr_keyword () = fg_256 51

let invalid_keyword () = fg_256 255 ^ bg_256 160

let static_keyword () = fg_256 255 ^ bg_256 240

let static_part () = fg_256 255 ^ bg_256 237

let continuation () = fg_256 35

let continuation_definition () = bg_256 237

let continuation_annotation () = fg_256 202 ^ bg_256 237

let name_abstraction () = fg_256 172

let rec_info () = fg_256 249

let coercion () = fg_256 249

let depth_variable () = fg_256 214

let error () = fg_256 160

let elide () = fg_256 243

let each_file () = fg_256 51

let lambda () = expr_keyword ()
