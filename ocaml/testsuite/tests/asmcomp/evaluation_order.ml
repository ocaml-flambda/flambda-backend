(* TEST
*)
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external caml_bytes_get_16 : bytes -> int -> int = "%caml_bytes_get16"

open Bigarray
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

external caml_bigstring_get_16 :
  bigstring -> int -> int = "%caml_bigstring_get16"

let bigstring_of_string s =
  let a = Array1.create char c_layout (String.length s) in
  for i = 0 to String.length s - 1 do
    a.{i} <- s.[i]
  done;
  a

let () =
  (* stringref_safe *)
  String.get (print_endline "hello"; "foo") (print_endline "world"; 0)
  |> Printf.printf "%c\n";

  (* string_load *)
  caml_bytes_get_16 (print_endline "hello"; Bytes.make 10 '\x00')
    (print_endline "world"; 0)
  |> Printf.printf "%x\n";

  (* bigstring_load *)
  caml_bigstring_get_16 (print_endline "hello";
                         bigstring_of_string (String.make 10 '\x00'))
    (print_endline "world"; 0)
  |> Printf.printf "%x\n";
  ()
