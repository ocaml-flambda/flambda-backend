(* TEST
   * expect
*)

let f = fun ~(src_pos:[%src_pos]) -> 
  print_endline (src_pos.pos_fname ^ (string_of_int src_pos.pos_lnum));;

(* TODO call the function with some stuff! *)
  [%%expect{||}]

(* type position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
} *)
(** A value of type [position] describes a point in a source file.
   [pos_fname] is the file name; [pos_lnum] is the line number;
   [pos_bol] is the offset of the beginning of the line (number
   of characters between the beginning of the lexbuf and the beginning
   of the line); [pos_cnum] is the offset of the position (number of
   characters between the beginning of the lexbuf and the position).
   The difference between [pos_cnum] and [pos_bol] is the character
   offset within the line (i.e. the column number, assuming each
   character is one column wide).

   See the documentation of type [lexbuf] for information about
   how the lexing engine will manage positions.
 *)