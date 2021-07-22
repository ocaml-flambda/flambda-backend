(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Stdlib = struct


external register_named_value : string -> 'a -> unit
                              = "caml_register_named_value"

let () =
  (* for runtime/fail_nat.c *)
  register_named_value "Pervasives.array_bound_error"
    (Invalid_argument "index out of bounds")

external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"

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

(* Composition operators *)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

(* Debugging *)

external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"

external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"

(* Comparisons *)

external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"

let min x y = if x <= y then x else y
let max x y = if x >= y then x else y

external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external ( & ) : bool -> bool -> bool = "%sequand"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( or ) : bool -> bool -> bool = "%sequor"
external ( || ) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( mod ) : int -> int -> int = "%modint"

let abs x = if x >= 0 then x else -x

external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"

let lnot x = x lxor (-1)

external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"

let max_int = (-1) lsr 1
let min_int = max_int + 1

(* Floating-point operations *)

external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
  [@@unboxed] [@@noalloc]
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
  [@@unboxed] [@@noalloc]
external acos : float -> float = "caml_acos_float" "acos"
  [@@unboxed] [@@noalloc]
external asin : float -> float = "caml_asin_float" "asin"
  [@@unboxed] [@@noalloc]
external atan : float -> float = "caml_atan_float" "atan"
  [@@unboxed] [@@noalloc]
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
  [@@unboxed] [@@noalloc]
external hypot : float -> float -> float
               = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
  [@@unboxed] [@@noalloc]
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
  [@@unboxed] [@@noalloc]
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
  [@@unboxed] [@@noalloc]
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
  [@@unboxed] [@@noalloc]
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
  [@@unboxed] [@@noalloc]
external ceil : float -> float = "caml_ceil_float" "ceil"
  [@@unboxed] [@@noalloc]
external floor : float -> float = "caml_floor_float" "floor"
  [@@unboxed] [@@noalloc]
external abs_float : float -> float = "%absfloat"
external copysign : float -> float -> float
                  = "caml_copysign_float" "caml_copysign"
                  [@@unboxed] [@@noalloc]
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
  [@@unboxed] [@@noalloc]
external frexp : float -> float * int = "caml_frexp_float"
external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) =
  "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]
let infinity =
  float_of_bits 0x7F_F0_00_00_00_00_00_00L
let neg_infinity =
  float_of_bits 0xFF_F0_00_00_00_00_00_00L
let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L
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
external classify_float : (float [@unboxed]) -> fpclass =
  "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]

(* String and byte sequence operations -- more in modules String and Bytes *)

external string_length : string -> int = "%string_length"
external bytes_length : bytes -> int = "%bytes_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external string_blit : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]
external bytes_blit : bytes -> int -> bytes -> int -> int -> unit
                        = "caml_blit_bytes" [@@noalloc]
external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = bytes_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  bytes_unsafe_to_string s

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
let char_of_int n =
  if n < 0 || n > 255 then invalid_arg "char_of_int" else unsafe_char_of_int n

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* References *)

type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"

(* Result type *)

type ('a,'b) result = Ok of 'a | Error of 'b

(* String conversion functions *)

external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"

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

external int_of_string : string -> int = "caml_int_of_string"

let int_of_string_opt s =
  (* TODO: provide this directly as a non-raising primitive. *)
  try Some (int_of_string s)
  with Failure _ -> None

external string_get : string -> int -> char = "%string_safe_get"

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

external float_of_string : string -> float = "caml_float_of_string"

let float_of_string_opt s =
  (* TODO: provide this directly as a non-raising primitive. *)
  try Some (float_of_string s)
  with Failure _ -> None

(* List operations -- more in module List *)

let rec ( @ ) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* I/O operations *)

type in_channel
type out_channel

external open_descriptor_out : int -> out_channel
                             = "caml_ml_open_descriptor_out"
external open_descriptor_in : int -> in_channel = "caml_ml_open_descriptor_in"

let stdin = open_descriptor_in 0
let stdout = open_descriptor_out 1
let stderr = open_descriptor_out 2

(* General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

external open_desc : string -> open_flag list -> int -> int = "caml_sys_open"

external set_out_channel_name: out_channel -> string -> unit =
  "caml_ml_set_channel_name"

let open_out_gen mode perm name =
  let c = open_descriptor_out(open_desc name mode perm) in
  set_out_channel_name c name;
  c

let open_out name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o666 name

let open_out_bin name =
  open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o666 name

external flush : out_channel -> unit = "caml_ml_flush"

external out_channels_list : unit -> out_channel list
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

external unsafe_output : out_channel -> bytes -> int -> int -> unit
                       = "caml_ml_output_bytes"
external unsafe_output_string : out_channel -> string -> int -> int -> unit
                              = "caml_ml_output"

external output_char : out_channel -> char -> unit = "caml_ml_output_char"

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

external output_byte : out_channel -> int -> unit = "caml_ml_output_char"
external output_binary_int : out_channel -> int -> unit = "caml_ml_output_int"

external marshal_to_channel : out_channel -> 'a -> unit list -> unit
     = "caml_output_value"
let output_value chan v = marshal_to_channel chan v []

external seek_out : out_channel -> int -> unit = "caml_ml_seek_out"
external pos_out : out_channel -> int = "caml_ml_pos_out"
external out_channel_length : out_channel -> int = "caml_ml_channel_size"
external close_out_channel : out_channel -> unit = "caml_ml_close_channel"
let close_out oc = flush oc; close_out_channel oc
let close_out_noerr oc =
  (try flush oc with _ -> ());
  (try close_out_channel oc with _ -> ())
external set_binary_mode_out : out_channel -> bool -> unit
                             = "caml_ml_set_binary_mode"

(* General input functions *)

external set_in_channel_name: in_channel -> string -> unit =
  "caml_ml_set_channel_name"

let open_in_gen mode perm name =
  let c = open_descriptor_in(open_desc name mode perm) in
  set_in_channel_name c name;
  c

let open_in name =
  open_in_gen [Open_rdonly; Open_text] 0 name

let open_in_bin name =
  open_in_gen [Open_rdonly; Open_binary] 0 name

external input_char : in_channel -> char = "caml_ml_input_char"

external unsafe_input : in_channel -> bytes -> int -> int -> int
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

external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"

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

external input_byte : in_channel -> int = "caml_ml_input_char"
external input_binary_int : in_channel -> int = "caml_ml_input_int"
external input_value : in_channel -> 'a = "caml_input_value"
external seek_in : in_channel -> int -> unit = "caml_ml_seek_in"
external pos_in : in_channel -> int = "caml_ml_pos_in"
external in_channel_length : in_channel -> int = "caml_ml_channel_size"
external close_in : in_channel -> unit = "caml_ml_close_channel"
let close_in_noerr ic = (try close_in ic with _ -> ())
external set_binary_mode_in : in_channel -> bool -> unit
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
    external seek_out : out_channel -> int64 -> unit = "caml_ml_seek_out_64"
    external pos_out : out_channel -> int64 = "caml_ml_pos_out_64"
    external out_channel_length : out_channel -> int64
                                = "caml_ml_channel_size_64"
    external seek_in : in_channel -> int64 -> unit = "caml_ml_seek_in_64"
    external pos_in : in_channel -> int64 = "caml_ml_pos_in_64"
    external in_channel_length : in_channel -> int64 = "caml_ml_channel_size_64"
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
 ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

let ( ^^ ) (Format (fmt1, str1)) (Format (fmt2, str2)) =
  Format (CamlinternalFormatBasics.concat_fmt fmt1 fmt2,
          str1 ^ "%," ^ str2)

(* Miscellaneous *)

external sys_exit : int -> 'a = "caml_sys_exit"

let exit_function = ref flush_all

let at_exit f =
  let g = !exit_function in
  (* MPR#7253, MPR#7796: make sure "f" is executed only once *)
  let f_already_ran = ref false in
  exit_function :=
    (fun () ->
      if not !f_already_ran then begin f_already_ran := true; f() end;
      g())

let do_at_exit () = (!exit_function) ()

let exit retcode =
  do_at_exit ();
  sys_exit retcode

let _ = register_named_value "Pervasives.do_at_exit" do_at_exit

end

open Stdlib
(* Pseudo-random number generator
   This is a lagged-Fibonacci F(55, 24, +) with a modified addition
   function to enhance the mixing of bits.
   If we use normal addition, the low-order bit fails tests 1 and 7
   of the Diehard test suite, and bits 1 and 2 also fail test 7.
   If we use multiplication as suggested by Marsaglia, it doesn't fare
   much better.
   By mixing the bits of one of the numbers before addition (XOR the
   5 high-order bits into the low-order bits), we get a generator that
   passes all the Diehard tests.
*)

external random_seed: unit -> int array = "caml_sys_random_seed"

module State = struct

  type t = { st : int array; mutable idx : int }

  let new_state () = { st = Array.make 55 0; idx = 0 }
  let assign st1 st2 =
    Array.blit st2.st 0 st1.st 0 55;
    st1.idx <- st2.idx


  let full_init s seed =
    let combine accu x = Digest.string (accu ^ Int.to_string x) in
    let extract d =
      Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16)
      + (Char.code d.[3] lsl 24)
    in
    let seed = if Array.length seed = 0 then [| 0 |] else seed in
    let l = Array.length seed in
    for i = 0 to 54 do
      s.st.(i) <- i;
    done;
    let accu = ref "x" in
    for i = 0 to 54 + max 55 l do
      let j = i mod 55 in
      let k = i mod l in
      accu := combine !accu seed.(k);
      s.st.(j) <- (s.st.(j) lxor extract !accu) land 0x3FFFFFFF;  (* PR#5575 *)
    done;
    s.idx <- 0


  let make seed =
    let result = new_state () in
    full_init result seed;
    result


  let make_self_init () = make (random_seed ())

  let copy s =
    let result = new_state () in
    assign result s;
    result


  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let bits s =
    s.idx <- (s.idx + 1) mod 55;
    let curval = s.st.(s.idx) in
    let newval = s.st.((s.idx + 24) mod 55)
                 + (curval lxor ((curval lsr 25) land 0x1F)) in
    let newval30 = newval land 0x3FFFFFFF in  (* PR#5575 *)
    s.st.(s.idx) <- newval30;
    newval30


  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound


  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound


  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound


  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))


  (* Returns a float 0 <= x <= 1 with at most 60 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0  (* 2^30 *)
    and r1 = Stdlib.float (bits s)
    and r2 = Stdlib.float (bits s)
    in (r1 /. scale +. r2) /. scale


  let float s bound = rawfloat s *. bound

  let bool s = (bits s land 1 = 0)

end

(* This is the state you get with [init 27182818] and then applying
   the "land 0x3FFFFFFF" filter to them.  See #5575, #5793, #5977. *)
let default = {
  State.st = [|
      0x3ae2522b; 0x1d8d4634; 0x15b4fad0; 0x18b14ace; 0x12f8a3c4; 0x3b086c47;
      0x16d467d6; 0x101d91c7; 0x321df177; 0x0176c193; 0x1ff72bf1; 0x1e889109;
      0x0b464b18; 0x2b86b97c; 0x0891da48; 0x03137463; 0x085ac5a1; 0x15d61f2f;
      0x3bced359; 0x29c1c132; 0x3a86766e; 0x366d8c86; 0x1f5b6222; 0x3ce1b59f;
      0x2ebf78e1; 0x27cd1b86; 0x258f3dc3; 0x389a8194; 0x02e4c44c; 0x18c43f7d;
      0x0f6e534f; 0x1e7df359; 0x055d0b7e; 0x10e84e7e; 0x126198e4; 0x0e7722cb;
      0x1cbede28; 0x3391b964; 0x3d40e92a; 0x0c59933d; 0x0b8cd0b7; 0x24efff1c;
      0x2803fdaa; 0x08ebc72e; 0x0f522e32; 0x05398edc; 0x2144a04c; 0x0aef3cbd;
      0x01ad4719; 0x35b93cd6; 0x2a559d4f; 0x1e6fd768; 0x26e27f36; 0x186f18c3;
      0x2fbf967a;
    |];
  State.idx = 0;
}

let bits () = State.bits default
let int bound = State.int default bound
let int32 bound = State.int32 default bound
let nativeint bound = State.nativeint default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default

let full_init seed = State.full_init default seed
let init seed = State.full_init default [| seed |]
let self_init () = full_init (random_seed())

(* Manipulating the current state. *)

let get_state () = State.copy default
let set_state s = State.assign default s

(********************

(* Test functions.  Not included in the library.
   The [chisquare] function should be called with n > 10r.
   It returns a triple (low, actual, high).
   If low <= actual <= high, the [g] function passed the test,
   otherwise it failed.

  Some results:

init 27182818; chisquare int 100000 1000
init 27182818; chisquare int 100000 100
init 27182818; chisquare int 100000 5000
init 27182818; chisquare int 1000000 1000
init 27182818; chisquare int 100000 1024
init 299792643; chisquare int 100000 1024
init 14142136; chisquare int 100000 1024
init 27182818; init_diff 1024; chisquare diff 100000 1024
init 27182818; init_diff 100; chisquare diff 100000 100
init 27182818; init_diff2 1024; chisquare diff2 100000 1024
init 27182818; init_diff2 100; chisquare diff2 100000 100
init 14142136; init_diff2 100; chisquare diff2 100000 100
init 299792643; init_diff2 100; chisquare diff2 100000 100
- : float * float * float = (936.754446796632465, 997.5, 1063.24555320336754)
# - : float * float * float = (80., 89.7400000000052387, 120.)
# - : float * float * float = (4858.57864376269, 5045.5, 5141.42135623731)
# - : float * float * float =
(936.754446796632465, 944.805999999982305, 1063.24555320336754)
# - : float * float * float = (960., 1019.19744000000355, 1088.)
# - : float * float * float = (960., 1059.31776000000536, 1088.)
# - : float * float * float = (960., 1039.98463999999512, 1088.)
# - : float * float * float = (960., 1054.38207999999577, 1088.)
# - : float * float * float = (80., 90.096000000005, 120.)
# - : float * float * float = (960., 1076.78720000000612, 1088.)
# - : float * float * float = (80., 85.1760000000067521, 120.)
# - : float * float * float = (80., 85.2160000000003492, 120.)
# - : float * float * float = (80., 80.6220000000030268, 120.)

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then Stdlib.float v.(i0) *. Stdlib.float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1


let chisquare g n r =
  if n <= 10 * r then invalid_arg "chisquare";
  let f = Array.make r 0 in
  for i = 1 to n do
    let t = g r in
    f.(t) <- f.(t) + 1
  done;
  let t = sumsq f 0 r
  and r = Stdlib.float r
  and n = Stdlib.float n in
  let sr = 2.0 *. sqrt r in
  (r -. sr,   (r *. t /. n) -. n,   r +. sr)


(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref 0
let init_diff r = st := int r
let diff r =
  let x1 = !st
  and x2 = int r
  in
  st := x2;
  if x1 >= x2 then
    x1 - x2
  else
    r + x1 - x2


let st1 = ref 0
and st2 = ref 0


(* This is to test for quadratic dependencies between successive random
   numbers.
*)
let init_diff2 r = st1 := int r; st2 := int r
let diff2 r =
  let x1 = !st1
  and x2 = !st2
  and x3 = int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r


********************)
