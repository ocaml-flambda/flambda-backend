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
(* Hash tables *)

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
external old_hash_param :
  int -> int -> 'a -> int = "caml_hash_univ_param" [@@noalloc]

let hash x = seeded_hash_param 10 100 0 x
let hash_param n1 n2 x = seeded_hash_param n1 n2 0 x
let seeded_hash seed x = seeded_hash_param 10 100 seed x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    mutable seed: int;                        (* for randomization *)
    mutable initial_size: int;                (* initial array size *)
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of { mutable key: 'a;
              mutable data: 'b;
              mutable next: ('a, 'b) bucketlist }

(* The sign of initial_size encodes the fact that a traversal is
   ongoing or not.

   This disables the efficient in place implementation of resizing.
*)

let ongoing_traversal h =
  Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
  || h.initial_size < 0

let flip_ongoing_traversal h =
  h.initial_size <- - h.initial_size

(* To pick random seeds if requested *)

let randomized_default =
  let params =
    try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
    try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
  String.contains params 'R'

let randomized = ref randomized_default

let randomize () = randomized := true
let is_randomized () = !randomized

let prng = lazy (Random.State.make_self_init())

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create ?(random = !randomized) initial_size =
  let s = power_2_above 16 initial_size in
  let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
  { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- Empty
  done

let reset h =
  let len = Array.length h.data in
  if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = abs h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) Empty
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons {key; data; next} ->
      let rec loop prec = function
        | Empty -> ()
        | Cons {key; data; next} ->
            let r = Cons {key; data; next} in
            begin match prec with
            | Empty -> assert false
            | Cons prec ->  prec.next <- r
            end;
            loop r next
      in
      let r = Cons {key; data; next} in
      loop r next;
      r

let copy h = { h with data = Array.map copy_bucketlist h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let ndata_tail = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
      | Empty -> ()
      | Cons {key; data; next} as cell ->
          let cell =
            if inplace then cell
            else Cons {key; data; next = Empty}
          in
          let nidx = indexfun h key in
          begin match ndata_tail.(nidx) with
          | Empty -> ndata.(nidx) <- cell;
          | Cons tail -> tail.next <- cell;
          end;
          ndata_tail.(nidx) <- cell;
          insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    if inplace then
      for i = 0 to nsize - 1 do
        match ndata_tail.(i) with
        | Empty -> ()
        | Cons tail -> tail.next <- Empty
      done;
  end

let key_index h key =
  (* compatibility with old hash tables *)
  if Obj.size (Obj.repr h) >= 3
  then (seeded_hash_param 10 100 h.seed key) land (Array.length h.data - 1)
  else (old_hash_param 10 100 key) mod (Array.length h.data)

let add h key data =
  let i = key_index h key in
  let bucket = Cons{key; data; next=h.data.(i)} in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize key_index h

let rec remove_bucket h i key prec = function
  | Empty ->
      ()
  | (Cons {key=k; next}) as c ->
      if compare k key = 0
      then begin
        h.size <- h.size - 1;
        match prec with
        | Empty -> h.data.(i) <- next
        | Cons c -> c.next <- next
      end
      else remove_bucket h i key c next

let remove h key =
  let i = key_index h key in
  remove_bucket h i key Empty h.data.(i)

let rec find_rec key = function
  | Empty ->
      raise Not_found
  | Cons{key=k; data; next} ->
      if compare key k = 0 then data else find_rec key next

let find h key =
  match h.data.(key_index h key) with
  | Empty -> raise Not_found
  | Cons{key=k1; data=d1; next=next1} ->
      if compare key k1 = 0 then d1 else
      match next1 with
      | Empty -> raise Not_found
      | Cons{key=k2; data=d2; next=next2} ->
          if compare key k2 = 0 then d2 else
          match next2 with
          | Empty -> raise Not_found
          | Cons{key=k3; data=d3; next=next3} ->
              if compare key k3 = 0 then d3 else find_rec key next3

let rec find_rec_opt key = function
  | Empty ->
      None
  | Cons{key=k; data; next} ->
      if compare key k = 0 then Some data else find_rec_opt key next

let find_opt h key =
  match h.data.(key_index h key) with
  | Empty -> None
  | Cons{key=k1; data=d1; next=next1} ->
      if compare key k1 = 0 then Some d1 else
      match next1 with
      | Empty -> None
      | Cons{key=k2; data=d2; next=next2} ->
          if compare key k2 = 0 then Some d2 else
          match next2 with
          | Empty -> None
          | Cons{key=k3; data=d3; next=next3} ->
              if compare key k3 = 0 then Some d3 else find_rec_opt key next3

let find_all h key =
  let rec find_in_bucket = function
  | Empty ->
      []
  | Cons{key=k; data; next} ->
      if compare k key = 0
      then data :: find_in_bucket next
      else find_in_bucket next in
  find_in_bucket h.data.(key_index h key)

let rec replace_bucket key data = function
  | Empty ->
      true
  | Cons ({key=k; next} as slot) ->
      if compare k key = 0
      then (slot.key <- key; slot.data <- data; false)
      else replace_bucket key data next

let replace h key data =
  let i = key_index h key in
  let l = h.data.(i) in
  if replace_bucket key data l then begin
    h.data.(i) <- Cons{key; data; next=l};
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize key_index h
  end

let mem h key =
  let rec mem_in_bucket = function
  | Empty ->
      false
  | Cons{key=k; next} ->
      compare k key = 0 || mem_in_bucket next in
  mem_in_bucket h.data.(key_index h key)

let iter f h =
  let rec do_bucket = function
    | Empty ->
        ()
    | Cons{key; data; next} ->
        f key data; do_bucket next in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done;
    if not old_trav then flip_ongoing_traversal h;
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec filter_map_inplace_bucket f h i prec = function
  | Empty ->
      begin match prec with
      | Empty -> h.data.(i) <- Empty
      | Cons c -> c.next <- Empty
      end
  | (Cons ({key; data; next} as c)) as slot ->
      begin match f key data with
      | None ->
          h.size <- h.size - 1;
          filter_map_inplace_bucket f h i prec next
      | Some data ->
          begin match prec with
          | Empty -> h.data.(i) <- slot
          | Cons c -> c.next <- slot
          end;
          c.data <- data;
          filter_map_inplace_bucket f h i slot next
      end

let filter_map_inplace f h =
  let d = h.data in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    for i = 0 to Array.length d - 1 do
      filter_map_inplace_bucket f h i Empty h.data.(i)
    done
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons{key; data; next} ->
        do_bucket next (f key data accu) in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    let accu = ref init in
    for i = 0 to Array.length d - 1 do
      accu := do_bucket d.(i) !accu
    done;
    if not old_trav then flip_ongoing_traversal h;
    !accu
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

let rec bucket_length accu = function
  | Empty -> accu
  | Cons{next} -> bucket_length (accu + 1) next

let stats h =
  let mbl =
    Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
      let l = bucket_length 0 b in
      histo.(l) <- histo.(l) + 1)
    h.data;
  { num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }

(** {1 Iterators} *)

let to_seq tbl =
  (* capture current array, so that even if the table is resized we
     keep iterating on the same array *)
  let tbl_data = tbl.data in
  (* state: index * next bucket to traverse *)
  let rec aux i buck () = match buck with
    | Empty ->
        if i = Array.length tbl_data
        then Seq.Nil
        else aux(i+1) tbl_data.(i) ()
    | Cons {key; data; next} ->
        Seq.Cons ((key, data), aux i next)
  in
  aux 0 Empty

let to_seq_keys m = Seq.map fst (to_seq m)

let to_seq_values m = Seq.map snd (to_seq m)

let add_seq tbl i =
  Seq.iter (fun (k,v) -> add tbl k v) i

let replace_seq tbl i =
  Seq.iter (fun (k,v) -> replace tbl k v) i

let of_seq i =
  let tbl = create 16 in
  replace_seq tbl i;
  tbl

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type SeededHashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: int -> t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_opt: 'a t -> key -> 'a option
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
    val stats: 'a t -> statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : _ t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

module type SeededS =
  sig
    type key
    type 'a t
    val create : ?random:bool -> int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt: 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : _ t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

module MakeSeeded(H: SeededHashedType): (SeededS with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.hash h.seed key) land (Array.length h.data - 1)

    let add h key data =
      let i = key_index h key in
      let bucket = Cons{key; data; next=h.data.(i)} in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h

    let rec remove_bucket h i key prec = function
      | Empty ->
          ()
      | (Cons {key=k; next}) as c ->
          if H.equal k key
          then begin
            h.size <- h.size - 1;
            match prec with
            | Empty -> h.data.(i) <- next
            | Cons c -> c.next <- next
          end
          else remove_bucket h i key c next

    let remove h key =
      let i = key_index h key in
      remove_bucket h i key Empty h.data.(i)

    let rec find_rec key = function
      | Empty ->
          raise Not_found
      | Cons{key=k; data; next} ->
          if H.equal key k then data else find_rec key next

    let find h key =
      match h.data.(key_index h key) with
      | Empty -> raise Not_found
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then d1 else
          match next1 with
          | Empty -> raise Not_found
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then d2 else
              match next2 with
              | Empty -> raise Not_found
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then d3 else find_rec key next3

    let rec find_rec_opt key = function
      | Empty ->
          None
      | Cons{key=k; data; next} ->
          if H.equal key k then Some data else find_rec_opt key next

    let find_opt h key =
      match h.data.(key_index h key) with
      | Empty -> None
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then Some d1 else
          match next1 with
          | Empty -> None
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then Some d2 else
              match next2 with
              | Empty -> None
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then Some d3 else find_rec_opt key next3

    let find_all h key =
      let rec find_in_bucket = function
      | Empty ->
          []
      | Cons{key=k; data=d; next} ->
          if H.equal k key
          then d :: find_in_bucket next
          else find_in_bucket next in
      find_in_bucket h.data.(key_index h key)

    let rec replace_bucket key data = function
      | Empty ->
          true
      | Cons ({key=k; next} as slot) ->
          if H.equal k key
          then (slot.key <- key; slot.data <- data; false)
          else replace_bucket key data next

    let replace h key data =
      let i = key_index h key in
      let l = h.data.(i) in
      if replace_bucket key data l then begin
        h.data.(i) <- Cons{key; data; next=l};
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h
      end

    let mem h key =
      let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons{key=k; next} ->
          H.equal k key || mem_in_bucket next in
      mem_in_bucket h.data.(key_index h key)

    let add_seq tbl i =
      Seq.iter (fun (k,v) -> add tbl k v) i

    let replace_seq tbl i =
      Seq.iter (fun (k,v) -> replace tbl k v) i

    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl

    let iter = iter
    let filter_map_inplace = filter_map_inplace
    let fold = fold
    let length = length
    let stats = stats
    let to_seq = to_seq
    let to_seq_keys = to_seq_keys
    let to_seq_values = to_seq_values
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl
  end
