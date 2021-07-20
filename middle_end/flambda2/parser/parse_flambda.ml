module Lex = Flambda_lex
module Parser = Flambda_parser

type error =
  | Lexing_error of Lex.error * Location.t
  | Parsing_error of string * Location.t

let add_pos (pos1 : Lexing.position) (pos2 : Lexing.position)
      : Lexing.position = {
  pos_fname = pos1.pos_fname;
  pos_lnum = pos1.pos_lnum + pos2.pos_lnum - 1;
  pos_bol = pos1.pos_bol + pos2.pos_bol;
  pos_cnum = pos1.pos_cnum + pos2.pos_cnum
}

let make_loc ?relative_to (startpos, endpos) = 
  let abs pos =
    match relative_to with
    | Some base -> add_pos base pos
    | None -> pos
  in 
  {
    Location.loc_start = abs startpos;
    Location.loc_end = abs endpos;
    Location.loc_ghost = false;
  }

let initial_pos filename =
  { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

let run_parser ~start_symbol ~start_pos (lb : Lexing.lexbuf) =
  let supplier =
    Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lex.token lb
  in
  (* [Lexing] assumes that the position it starts in has cnum = bol = 0, so
     we humor it and then add [start_pos] back in if there's an error. *)
  let pos = initial_pos Lexing.(start_pos.pos_fname) in
  let start = start_symbol pos in
  try
    Parser.MenhirInterpreter.loop_handle
      (fun ans -> Ok ans)
      (function
        | HandlingError error_state ->
          let s =
            Parser.MenhirInterpreter.current_state_number error_state
          in
          let msg =
            try Flambda_parser_messages.message s
            with Not_found -> Format.sprintf "Unknown error in state %d" s
          in
          (* CR-someday lmaurer: Fix the error messages. This is not a small
           * task - there are (or should be) 175 of them as of this writing,
           * many of them need to be rewritten entirely, and even for the ones
           * that need small tweaks, I don't know of any good way of updating
           * the comments in the .messages file that tell you the state of
           * the parser corresponding to an error message.
           *
           * A good alternative would be to generate something sensible
           * automatically by inspecting the grammar, but I don't see any
           * straightforward way to do that either. *)
          let msg = msg ^ " (note: error messages are wildly out of date)" in
          let loc =
            make_loc ~relative_to:start_pos
              (Parser.MenhirInterpreter.positions error_state)
          in
          Error (Parsing_error (msg, loc))
        | _ ->
          assert false (* the manual promises that HandlingError is the
          only possible constructor *))
      supplier start
  with
  | Lex.Error (error, loc) ->
    Error (Lexing_error (error, make_loc ~relative_to:start_pos loc))

let run_parser_on_file ~start_symbol filename =
  let ic = open_in filename in
  Misc.try_finally ~always:(fun () -> close_in ic) (fun () ->
    let start_pos = initial_pos filename in
    let lb = Lexing.from_channel ic in
    run_parser ~start_symbol ~start_pos lb
  )

let parse_fexpr filename =
  run_parser_on_file ~start_symbol:Parser.Incremental.flambda_unit filename

let parse_expect_test_spec filename =
  run_parser_on_file ~start_symbol:Parser.Incremental.expect_test_spec filename

let text_of_reversed_lines rev_lines =
  let rev_lines = "" :: rev_lines in (* Add terminating newline to last line *)
  String.concat "\n" (List.rev rev_lines)

let after_line ~length (pos : Lexing.position) =
  let pos_bol = pos.pos_bol + length in (* include the newline *)
  { pos with pos_lnum = pos.pos_lnum + 1; pos_bol; pos_cnum = pos_bol }

let read_lines ~until ~pos ic =
  let rec loop pos rev_lines =
    match input_line ic with
    | line ->
      let pos = after_line pos ~length:(String.length line) in
      if String.equal line until
        then true, pos, text_of_reversed_lines rev_lines
        else loop pos (line :: rev_lines)
    | exception End_of_file ->
      false, pos, text_of_reversed_lines rev_lines
  in
  loop pos []

let parse_markdown_doc filename =
  let ic = open_in filename in
  Misc.try_finally ~always:(fun () -> close_in ic) (fun () ->
    let rec read_text pos rev_nodes =
      let matched_block_start, pos, text =
        read_lines ic ~until:"```flexpect" ~pos
      in
      let rev_nodes =
        match text with
        | "" -> rev_nodes
        | _ -> (Text text : Fexpr.markdown_node) :: rev_nodes
      in
      if matched_block_start
        then read_block pos rev_nodes
        else Ok (List.rev rev_nodes)
    and read_block pos rev_nodes =
      let matched_block_end, end_pos, text =
        read_lines ic ~until:"```" ~pos
      in
      if not matched_block_end
        then
          let loc = make_loc (end_pos, end_pos)  in
          Error (Parsing_error ("Unexpected EOF", loc))
        else
          Result.bind (
            run_parser
              ~start_symbol:Parser.Incremental.expect_test_spec
              ~start_pos:pos
              (Lexing.from_string text)
          ) (fun test_spec ->
            let rev_nodes : Fexpr.markdown_node list =
              Expect test_spec :: rev_nodes
            in
            read_text end_pos rev_nodes
          )
    in
    let pos = initial_pos filename in
    read_text pos []
  )
;;
    
let make_compilation_unit ~extension ~filename ?(tag = "") () =
  let basename =
    Filename.chop_suffix filename extension
    |> Filename.basename
  in
  let name = String.capitalize_ascii basename ^ tag in
  (* CR lmaurer: Adding "caml" to the front is a hacky way to conform to the
     simplifier, which breaks when creating the module block symbol unless the
     compilation unit has exactly this linkage name. It would be better to
     either have the simplifier use the current compilation unit or not
     duplicate the prefixing logic here. *)
  let linkage_name = Linkage_name.create ("caml" ^ name) in
  let id = Ident.create_persistent name in
  Compilation_unit.create id linkage_name

let parse ~backend filename =
  parse_fexpr filename
  |> Result.map (fun fexpr ->
    let comp_unit = make_compilation_unit ~extension:".fl" ~filename () in
    let old_comp_unit = Compilation_unit.get_current () in
    Compilation_unit.set_current comp_unit;
    let module_ident = Compilation_unit.get_persistent_ident comp_unit in
    let flambda = Fexpr_to_flambda.conv ~backend ~module_ident fexpr in
    begin
      match old_comp_unit with
      | Some old_comp_unit -> Compilation_unit.set_current old_comp_unit
      | None -> ()
    end;
    flambda
  )
