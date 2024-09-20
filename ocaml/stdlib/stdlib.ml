(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "-49"]
[@@@ocaml.flambda_o3]

(* Exceptions *)

external register_named_value : string -> 'a -> unit @@ portable
                              = "caml_register_named_value"

let () =
  (* for runtime/fail_nat.c *)
  register_named_value "Pervasives.array_bound_error"
    (Invalid_argument "index out of bounds");
  register_named_value "Pervasives.array_align_error"
    (Invalid_argument "address was misaligned")

external raise : exn -> 'a @@ portable = "%reraise"
external raise_notrace : exn -> 'a @@ portable = "%raise_notrace"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

exception Exit
exception Match_failure = Match_failure
exception Assert_failure = Assert_failure
exception Invalid_argument = Invalid_argument
exception Failure = Failure
exception Not_found = Not_found
exception Out_of_memory = Out_of_memory
exception Stack_overflow = Stack_overflow
exception Sys_error = Sys_error
exception End_of_file = End_of_file
exception Division_by_zero = Division_by_zero
exception Sys_blocked_io = Sys_blocked_io
exception Undefined_recursive_module = Undefined_recursive_module

(* Magic *)

external magic_portable : 'a -> 'a @ portable @@ portable = "%identity"
external magic_uncontended : 'a @ contended -> 'a @@ portable = "%identity"

(* Composition operators *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b @@ portable = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b @@ portable = "%apply"

(* Debugging *)

external __LOC__ : string @@ portable = "%loc_LOC"
external __FILE__ : string @@ portable = "%loc_FILE"
external __LINE__ : int @@ portable = "%loc_LINE"
external __MODULE__ : string @@ portable = "%loc_MODULE"
external __POS__ : string * int * int * int @@ portable = "%loc_POS"
external __FUNCTION__ : string @@ portable = "%loc_FUNCTION"

external __LOC_OF__ : 'a -> string * 'a @@ portable = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a @@ portable = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a @@ portable = "%loc_POS"

(* Comparisons *)

external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%equal"
external ( <> ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%notequal"
external ( < ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%lessthan"
external ( > ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%greaterthan"
external ( <= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%lessequal"
external ( >= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%greaterequal"
external compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int @@ portable = "%compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external ( == ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%eq"
external ( != ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%noteq"

(* Boolean operations *)

external not : (bool[@local_opt]) -> bool @@ portable = "%boolnot"
external ( && ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool @@ portable = "%sequand"
external ( || ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool @@ portable = "%sequor"

(* Integer operations *)

external ( ~- ) : (int[@local_opt]) -> int @@ portable = "%negint"
external ( ~+ ) : (int[@local_opt]) -> int @@ portable = "%identity"
external succ : (int[@local_opt]) -> int @@ portable = "%succint"
external pred : (int[@local_opt]) -> int @@ portable = "%predint"
external ( + ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%addint"
external ( - ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%subint"
external ( * ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%mulint"
external ( / ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%divint"
external ( mod ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%modint"

let abs x = if x >= 0 then x else -x

external ( land ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%andint"
external ( lor ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%orint"
external ( lxor ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%xorint"

let lnot x = x lxor (-1)

external ( lsl ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%lslint"
external ( lsr ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%lsrint"
external ( asr ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%asrint"

let max_int = (-1) lsr 1
let min_int = max_int + 1

(* Floating-point operations *)

external ( ~-. ) : (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%negfloat"
external ( ~+. ) : (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%identity"
external ( +. ) : (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%addfloat"
external ( -. ) : (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%subfloat"
external ( *. ) : (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%mulfloat"
external ( /. ) : (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%divfloat"
external ( ** ) : float -> float -> float @@ portable = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external exp : float -> float @@ portable = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external expm1 : float -> float @@ portable = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]
external acos : float -> float @@ portable = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]
external asin : float -> float @@ portable = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]
external atan : float -> float @@ portable = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]
external atan2 : float -> float -> float @@ portable = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float @@ portable
               = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
external cos : float -> float @@ portable = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external cosh : float -> float @@ portable = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]
external acosh : float -> float @@ portable = "caml_acosh_float" "caml_acosh"
  [@@unboxed] [@@noalloc]
external log : float -> float @@ portable = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float @@ portable = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]
external log1p : float -> float @@ portable = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]
external sin : float -> float @@ portable = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external sinh : float -> float @@ portable = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]
external asinh : float -> float @@ portable = "caml_asinh_float" "caml_asinh"
  [@@unboxed] [@@noalloc]
external sqrt : float -> float @@ portable = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]
external tan : float -> float @@ portable = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external tanh : float -> float @@ portable = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]
external atanh : float -> float @@ portable = "caml_atanh_float" "caml_atanh"
  [@@unboxed] [@@noalloc]
external ceil : float -> float @@ portable = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc]
external floor : float -> float @@ portable = "caml_floor_float" "floor"
  [@@unboxed] [@@noalloc]
external abs_float : (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%absfloat"
external copysign : float -> float -> float @@ portable
                  = "caml_copysign_float" "caml_copysign"
                  [@@unboxed] [@@noalloc]
external mod_float : float -> float -> float @@ portable = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]
external frexp : float -> float * int @@ portable = "caml_frexp_float"
external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) @@ portable =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
external modf : float -> float * float @@ portable = "caml_modf_float"
external float : (int[@local_opt]) -> (float[@local_opt]) @@ portable = "%floatofint"
external float_of_int : (int[@local_opt]) -> (float[@local_opt]) @@ portable = "%floatofint"
external truncate : (float[@local_opt]) -> int @@ portable = "%intoffloat"
external int_of_float : (float[@local_opt]) -> int @@ portable = "%intoffloat"
external float_of_bits : int64 -> float @@ portable
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F8_00_00_00_00_00_01L
let max_float =
  float_of_bits 0x7F_EF_FF_FF_FF_FF_FF_FFL
let min_float =
  float_of_bits 0x00_10_00_00_00_00_00_00L
let epsilon_float =
  float_of_bits 0x3C_B0_00_00_00_00_00_00L

type fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float : (float [@unboxed]) -> fpclass @@ portable =
  "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]

(* String and byte sequence operations -- more in modules String and Bytes *)

external string_length : string -> int @@ portable = "%string_length"
external bytes_length : bytes -> int @@ portable = "%bytes_length"
external bytes_create : int -> bytes @@ portable = "caml_create_bytes"
external string_blit : string -> int -> bytes -> int -> int -> unit @@ portable
                     = "caml_blit_string" [@@noalloc]
external bytes_blit : bytes -> int -> bytes -> int -> int -> unit @@ portable
                        = "caml_blit_bytes" [@@noalloc]
external bytes_unsafe_to_string : bytes -> string @@ portable = "%bytes_to_string"

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = bytes_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  bytes_unsafe_to_string s

(* Character operations -- more in module Char *)

external int_of_char : char -> int @@ portable = "%identity"
external unsafe_char_of_int : int -> char @@ portable = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit @@ portable = "%ignore"

(* Pair operations *)

external fst : ('a * 'b[@local_opt]) -> ('a[@local_opt]) @@ portable = "%field0_immut"
external snd : ('a * 'b[@local_opt]) -> ('b[@local_opt]) @@ portable = "%field1_immut"

(* References *)

type 'a ref = { mutable contents : 'a }
external ref : 'a -> ('a ref[@local_opt]) @@ portable = "%makemutable"
external ( ! ) : ('a ref[@local_opt]) -> 'a @@ portable = "%field0"
external ( := ) : ('a ref[@local_opt]) -> 'a -> unit @@ portable = "%setfield0"
external incr : (int ref[@local_opt]) -> unit @@ portable = "%incr"
external decr : (int ref[@local_opt]) -> unit @@ portable = "%decr"

(* Result type *)

type ('a,'b) result = Ok of 'a | Error of 'b

(* String conversion functions *)

external format_int : string -> int -> string @@ portable = "caml_format_int"
external format_float : string -> float -> string @@ portable = "caml_format_float"

let string_of_bool b =
  if b then "true" else "false"
let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> invalid_arg "bool_of_string"

let bool_of_string_opt = function
  | "true" -> Some true
  | "false" -> Some false
  | _ -> None

let string_of_int n =
  format_int "%d" n

external int_of_string : string -> int @@ portable = "caml_int_of_string"

let int_of_string_opt s =
  (* TODO: provide this directly as a non-raising primitive. *)
  try Some (int_of_string s)
  with Failure _ -> None

external string_get : string -> int -> char @@ portable = "%string_safe_get"

let valid_float_lexem s =
  let l = string_length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match string_get s i with
    | '0' .. '9' | '-' -> loop (i + 1)
    | _ -> s
  in
  loop 0

let string_of_float f = valid_float_lexem (format_float "%.12g" f)

external float_of_string : string -> float @@ portable = "caml_float_of_string"

let float_of_string_opt s =
  (* TODO: provide this directly as a non-raising primitive. *)
  try Some (float_of_string s)
  with Failure _ -> None

(* List operations -- more in module List *)

let[@tail_mod_cons] rec ( @ ) l1 l2 =
  match l1 with
  | [] -> l2
  | h1 :: [] -> h1 :: l2
  | h1 :: h2 :: [] -> h1 :: h2 :: l2
  | h1 :: h2 :: h3 :: tl -> h1 :: h2 :: h3 :: (tl @ l2)

(* I/O operations *)

type in_channel : value mod portable uncontended
type out_channel : value mod portable uncontended

external open_descriptor_out : int -> out_channel @@ portable
                             = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel @@ portable = "caml_ml_open_descriptor_in"

let stdin = open_descriptor_in 0
let stdout = open_descriptor_out 1
let stderr = open_descriptor_out 2

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc : string -> open_flag list -> int -> int @@ portable = "caml_sys_open"

external set_out_channel_name: out_channel -> string -> unit @@ portable =
  "caml_ml_set_channel_name"

let open_out_gen mode perm name =
  let c = open_descriptor_out(open_desc name mode perm) in
  set_out_channel_name c name;
  c

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

external flush : out_channel -> unit @@ portable = "caml_ml_flush"

external out_channels_list : unit -> out_channel list @@ portable
                           = "caml_ml_out_channels_list"

let flush_all () =
  let rec iter = function
      [] -> ()
    | a::l ->
        begin try
            flush a
        with Sys_error _ ->
          () (* ignore channels closed during a preceding flush. *)
        end;
        iter l
  in iter (out_channels_list ())

external unsafe_output : out_channel -> bytes -> int -> int -> unit @@ portable
                       = "caml_ml_output_bytes"
external unsafe_output_string : out_channel -> string -> int -> int -> unit @@ portable
                              = "caml_ml_output"

external output_char : out_channel -> char -> unit @@ portable = "caml_ml_output_char"

let output_bytes oc s =
  unsafe_output oc s 0 (bytes_length s)

let output_string oc s =
  unsafe_output_string oc s 0 (string_length s)

let output oc s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "output"
  else unsafe_output oc s ofs len

let output_substring oc s ofs len =
  if ofs < 0 || len < 0 || ofs > string_length s - len
  then invalid_arg "output_substring"
  else unsafe_output_string oc s ofs len

external output_byte : out_channel -> int -> unit @@ portable = "caml_ml_output_char"
external output_binary_int : out_channel -> int -> unit @@ portable = "caml_ml_output_int"

external marshal_to_channel : out_channel -> 'a -> unit list -> unit @@ portable
     = "caml_output_value"
let output_value chan v = marshal_to_channel chan v []

external seek_out : out_channel -> int -> unit @@ portable = "caml_ml_seek_out"
external pos_out : out_channel -> int @@ portable = "caml_ml_pos_out"
external out_channel_length : out_channel -> int @@ portable = "caml_ml_channel_size"
external close_out_channel : out_channel -> unit @@ portable = "caml_ml_close_channel"
let close_out oc = flush oc; close_out_channel oc
let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())
external set_binary_mode_out : out_channel -> bool -> unit @@ portable
                             = "caml_ml_set_binary_mode"

(* General input functions *)

external set_in_channel_name: in_channel -> string -> unit @@ portable =
  "caml_ml_set_channel_name"

let open_in_gen mode perm name =
  let c = open_descriptor_in(open_desc name mode perm) in
  set_in_channel_name c name;
  c

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external input_char : in_channel -> char @@ portable = "caml_ml_input_char"

external unsafe_input : in_channel -> bytes -> int -> int -> int @@ portable
                      = "caml_ml_input"

let input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "input"
  else unsafe_input ic s ofs len

let rec unsafe_really_input ic s ofs len =
  if len <= 0 then () else begin
    let r = unsafe_input ic s ofs len in
    if r = 0
    then raise End_of_file
    else unsafe_really_input ic s (ofs + r) (len - r)
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs > bytes_length s - len
  then invalid_arg "really_input"
  else unsafe_really_input ic s ofs len

let really_input_string ic len =
  let s = bytes_create len in
  really_input ic s 0 len;
  bytes_unsafe_to_string s

external input_scan_line : in_channel -> int @@ portable = "caml_ml_input_scan_line"

let input_line chan =
  let rec build_result buf pos = function
    [] -> buf
  | hd :: tl ->
      let len = bytes_length hd in
      bytes_blit hd 0 buf (pos - len) len;
      build_result buf (pos - len) tl in
  let rec scan accu len =
    let n = input_scan_line chan in
    if n = 0 then begin                   (* n = 0: we are at EOF *)
      match accu with
        [] -> raise End_of_file
      | _  -> build_result (bytes_create len) len accu
    end else if n > 0 then begin          (* n > 0: newline found in buffer *)
      let res = bytes_create (n - 1) in
      ignore (unsafe_input chan res 0 (n - 1));
      ignore (input_char chan);           (* skip the newline *)
      match accu with
        [] -> res
      |  _ -> let len = len + n - 1 in
              build_result (bytes_create len) len (res :: accu)
    end else begin                        (* n < 0: newline not found *)
      let beg = bytes_create (-n) in
      ignore(unsafe_input chan beg 0 (-n));
      scan (beg :: accu) (len - n)
    end
  in bytes_unsafe_to_string (scan [] 0)

external input_byte : in_channel -> int @@ portable = "caml_ml_input_char"
external input_binary_int : in_channel -> int @@ portable = "caml_ml_input_int"
external input_value : in_channel -> 'a @@ portable = "caml_input_value"
external seek_in : in_channel -> int -> unit @@ portable = "caml_ml_seek_in"
external pos_in : in_channel -> int @@ portable = "caml_ml_pos_in"
external in_channel_length : in_channel -> int @@ portable = "caml_ml_channel_size"
external close_in : in_channel -> unit @@ portable = "caml_ml_close_channel"
let close_in_noerr ic = (try close_in ic with _ -> ())
external set_binary_mode_in : in_channel -> bool -> unit @@ portable
                            = "caml_ml_set_binary_mode"

(* Output functions on standard output *)

let print_char c = output_char stdout c
let print_string s = output_string stdout s
let print_bytes s = output_bytes stdout s
let print_int i = output_string stdout (string_of_int i)
let print_float f = output_string stdout (string_of_float f)
let print_endline s =
  output_string stdout s; output_char stdout '\n'; flush stdout
let print_newline () = output_char stdout '\n'; flush stdout

(* Output functions on standard error *)

let prerr_char c = output_char stderr c
let prerr_string s = output_string stderr s
let prerr_bytes s = output_bytes stderr s
let prerr_int i = output_string stderr (string_of_int i)
let prerr_float f = output_string stderr (string_of_float f)
let prerr_endline s =
  output_string stderr s; output_char stderr '\n'; flush stderr
let prerr_newline () = output_char stderr '\n'; flush stderr

(* Input functions on standard input *)

let read_line () = flush stdout; input_line stdin
let read_int () = int_of_string(read_line())
let read_int_opt () = int_of_string_opt(read_line())
let read_float () = float_of_string(read_line())
let read_float_opt () = float_of_string_opt(read_line())

(* Operations on large files *)

module LargeFile =
  struct
    external seek_out : out_channel -> int64 -> unit @@ portable = "caml_ml_seek_out_64"
    external pos_out : out_channel -> int64 @@ portable = "caml_ml_pos_out_64"
    external out_channel_length : out_channel -> int64 @@ portable
                                = "caml_ml_channel_size_64"
    external seek_in : in_channel -> int64 -> unit @@ portable = "caml_ml_seek_in_64"
    external pos_in : in_channel -> int64 @@ portable = "caml_ml_pos_in_64"
    external in_channel_length : in_channel -> int64 @@ portable = "caml_ml_channel_size_64"
  end

(* Formats *)

type ('a, 'b, 'c, 'd, 'e, 'f) format6
   = ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6
   = Format of ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.fmt
               * string

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

let string_of_format (Format (_fmt, str)) = str

external format_of_string :
 ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
 ('a, 'b, 'c, 'd, 'e, 'f) format6 @@ portable = "%identity"

let ( ^^ ) (Format (fmt1, str1)) (Format (fmt2, str2)) =
  Format (CamlinternalFormatBasics.concat_fmt fmt1 fmt2,
          str1 ^ "%," ^ str2)

(* Miscellaneous *)

external sys_exit : int -> 'a @@ portable = "caml_sys_exit"

(* for at_exit *)
type (!'a : value mod portable) atomic_t : value mod portable uncontended
external atomic_make : 'a -> 'a atomic_t @@ portable = "%makemutable"
external atomic_get : 'a atomic_t -> 'a @ contended @@ portable = "%atomic_load"
external atomic_exchange : 'a atomic_t -> 'a @ contended -> 'a @ contended @@ portable = "%atomic_exchange"
external atomic_compare_and_set : 'a atomic_t -> 'a @ contended -> 'a @ contended -> bool @@ portable
  = "%atomic_cas"

type exit_function : value mod portable = Exit_function of (unit -> unit) @@ portable

let exit_function = atomic_make (Exit_function flush_all)

let rec at_exit_safe f =
  (* MPR#7253, MPR#7796: make sure "f" is executed only once *)
  let f_yet_to_run = atomic_make true in
  let Exit_function old_exit = atomic_get exit_function in
  let new_exit () =
    if atomic_compare_and_set f_yet_to_run true false then f () ;
    old_exit ()
  in
  let success = atomic_compare_and_set exit_function (Exit_function old_exit) (Exit_function new_exit) in
  if not success then at_exit_safe f

let at_exit f = at_exit_safe (magic_portable f)

type at_exit : value mod portable = At_exit of (unit -> unit) @@ portable

let do_domain_local_at_exit = magic_portable (ref (fun () -> ()))

let do_domain_local_at_exit_safe = atomic_make (At_exit (fun () -> ()))

let set_do_domain_local_at_exit f =
  let _ = atomic_exchange do_domain_local_at_exit_safe (At_exit f) in
  ()

let do_at_exit () =
  (!(magic_uncontended do_domain_local_at_exit)) ();
  let At_exit do_domain_local_at_exit_safe = atomic_get do_domain_local_at_exit_safe in
  do_domain_local_at_exit_safe ();
  let Exit_function exit_function = atomic_get exit_function in
  exit_function ()

let exit retcode =
  do_at_exit ();
  sys_exit retcode

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit

external major : unit -> unit @@ portable = "caml_gc_major"
external naked_pointers_checked : unit -> bool @@ portable
  = "caml_sys_const_naked_pointers_checked"
let () = if naked_pointers_checked () then at_exit major

(*MODULE_ALIASES*)
module Arg            = Arg
module Array          = Array
module ArrayLabels    = ArrayLabels
module Atomic         = Atomic
module Bigarray       = Bigarray
module Bool           = Bool
module Buffer         = Buffer
module Bytes          = Bytes
module BytesLabels    = BytesLabels
module Callback       = Callback
module Char           = Char
module Complex        = Complex
module Condition      = Condition
module Digest         = Digest
module Domain         = Domain
module Effect         = Effect
module Either         = Either
module Ephemeron      = Ephemeron
module Filename       = Filename
module Float          = Float
module Format         = Format
module Fun            = Fun
module Gc             = Gc
module Hashtbl        = Hashtbl
module In_channel     = In_channel
module Int            = Int
module Int32          = Int32
module Int64          = Int64
module Lazy           = Lazy
module Lexing         = Lexing
module List           = List
module ListLabels     = ListLabels
module Map            = Map
module Marshal        = Marshal
module MoreLabels     = MoreLabels
module Mutex          = Mutex
module Nativeint      = Nativeint
module Obj            = Obj
module Oo             = Oo
module Option         = Option
module Out_channel    = Out_channel
module Parsing        = Parsing
module Printexc       = Printexc
module Printf         = Printf
module Queue          = Queue
module Random         = Random
module Result         = Result
module Scanf          = Scanf
module Semaphore      = Semaphore
module Seq            = Seq
module Set            = Set
module Stack          = Stack
module StdLabels      = StdLabels
module String         = String
module StringLabels   = StringLabels
module Sys            = Sys
module Type           = Type
module Uchar          = Uchar
module Unit           = Unit
module Weak           = Weak
