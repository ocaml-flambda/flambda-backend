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

(* Whether to output an up arrow on push and a down arrow on pop. Handy for
   hunting down mismatched pushes and pops. *)
let debug_push_and_pop = false

type directive = Format.formatter -> unit

let disable_colours = ref false

let is_colour_enabled =
  let colour_enabled =
    lazy
      ((* This avoids having to alter misc.ml *)
       let buf = Buffer.create 10 in
       let ppf = Format.formatter_of_buffer buf in
       Misc.Color.set_color_tag_handling ppf;
       Format.fprintf ppf "@{<error>@}%!";
       String.length (Buffer.contents buf) > 0)
  in
  fun () -> Lazy.force colour_enabled && not !disable_colours

let without_colours ~f =
  let tmp = !disable_colours in
  disable_colours := true;
  let res = f () in
  disable_colours := tmp;
  res

type state =
  { fg : int option;
    bg : int option
  }

let initial_state = { fg = None; bg = None }

let state_stack =
  (* Instead of an actual empty stack, we start with the initial state on top,
     since this makes [push] and [pop] simpler. *)
  ref [initial_state]

let output ppf str =
  if is_colour_enabled () then Format.fprintf ppf "@<0>%s" str

let sequence command_code arg =
  Printf.sprintf "\x1b[%d;5;%d;1m" command_code arg

let fg_256 n ppf = output ppf (sequence 38 n)

let bg_256 n ppf = output ppf (sequence 48 n)

let reset_out ppf = output ppf "\x1b[0m"

let render_state state ppf =
  reset_out ppf;
  Option.iter (fun fg -> fg_256 fg ppf) state.fg;
  Option.iter (fun bg -> bg_256 bg ppf) state.bg

let pop ppf =
  match !state_stack with
  | [] | [_] -> Misc.fatal_error "Flambda_colours.pop: too many pops"
  | _ :: (top_state :: _ as states) ->
    state_stack := states;
    render_state top_state ppf;
    if debug_push_and_pop then output ppf "\u{2193}"

let push ?fg ?bg ppf =
  let current_state =
    match !state_stack with
    | [] -> Misc.fatal_error "Flambda_colours.push_modified: empty stack"
    | state :: _ -> state
  in
  let update new_value old_value =
    match new_value with None -> old_value | Some _ -> new_value
  in
  let new_state =
    { fg = update fg current_state.fg; bg = update bg current_state.bg }
  in
  state_stack := new_state :: !state_stack;
  render_state new_state ppf;
  if debug_push_and_pop then output ppf "\u{2191}"

let none ppf = push ppf

let prim_constructive ppf = push ~fg:163 ppf

let prim_destructive ppf = push ~fg:62 ppf

let prim_neither ppf = push ~fg:130 ppf

let naked_number ppf = push ~fg:70 ppf

let unboxed_product ppf = push ~fg:198 ppf

let tagged_immediate ppf = push ~fg:70 ppf

let constructor ppf = push ~fg:69 ppf

let kind ppf = push ~fg:37 ppf

let subkind ppf = push ~fg:39 ppf

let top_or_bottom_type ppf = push ~fg:37 ppf

let debuginfo ppf = push ~fg:243 ppf

let discriminant ppf = push ~fg:111 ppf

let name ppf = push ~fg:111 ppf

let parameter ppf = push ~fg:198 ppf

let symbol ppf = push ~fg:98 ppf

let variable ppf = push ~fg:111 ppf

let function_slot ppf = push ~fg:31 ppf

let value_slot ppf = push ~fg:43 ppf

let code_id ppf = push ~fg:169 ppf

let expr_keyword ppf = push ~fg:51 ppf

let invalid_keyword ppf = push ~fg:255 ~bg:160 ppf

let static_keyword ppf = push ~fg:255 ~bg:240 ppf

let static_part ppf = push ~fg:255 ~bg:237 ppf

let continuation ppf = push ~fg:35 ppf

let continuation_definition ppf = push ~bg:237 ppf

let continuation_annotation ppf = push ~fg:202 ~bg:237 ppf

let name_abstraction ppf = push ~fg:172 ppf

let rec_info ppf = push ~fg:249 ppf

let coercion ppf = push ~fg:249 ppf

let depth_variable ppf = push ~fg:214 ppf

let error ppf = push ~fg:160 ppf

let elide ppf = push ~fg:243 ppf

let each_file ppf = push ~fg:51 ppf

let lambda = expr_keyword
