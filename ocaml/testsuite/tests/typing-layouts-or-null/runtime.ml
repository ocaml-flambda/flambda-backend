(* TEST
   flags = "-extension layouts_alpha";
   runtime5;
   native;
*)

let x = Null

let () =
  match x with
  | Null -> ()
  | This _ -> assert false
;;

let y = This 3

let () =
  match y with
  | This 3 -> ()
  | _ -> assert false
;;


external int_as_pointer : int -> int or_null = "%int_as_pointer"

let n = int_as_pointer 0

let () =
  match n with
  | Null -> ()
  | _ -> assert false
;;

external int_as_int : int -> int or_null = "%identity"

let m = int_as_int 5

let () =
  match m with
  | This 5 -> ()
  | This _ -> assert false
  | Null -> assert false
;;

let x = (Null, This "bar")

let () =
  match x with
  | Null, This "foo" -> assert false
  | Null, This "bar" -> ()
  | _, This "bar" -> assert false
  | Null, _ -> assert false
  | _, _ -> assert false
;;

let y a = fun () -> This a

let d = y 5

let () =
  match d () with
  | This 5 -> ()
  | _ -> assert false
;;

external to_bytes : ('a : value_or_null) . 'a -> int list -> bytes = "caml_output_value_to_bytes"

external from_bytes_unsafe : ('a : value_or_null) . bytes -> int -> 'a = "caml_input_value_from_bytes"

let z = to_bytes (This "foo") []

let () =
  match from_bytes_unsafe z 0 with
    | This "foo" -> ()
    | This _ -> assert false
    | Null -> assert false
;;

let w = to_bytes Null []

let () =
  match from_bytes_unsafe w 0 with
    | Null -> ()
    | This _ -> assert false
;;

external evil : 'a or_null -> 'a = "%identity"

let e = This (evil Null)

let () =
  match e with
  | Null -> ()
  | This _ -> assert false
;;

let e' = evil (This 4)

let () =
  match e' with
  | 4 -> ()
  | _ -> assert false
;;

let f a = fun () ->
  match a with
  | This x -> x ^ "bar"
  | Null -> "foo"
;;

let g = f (This "xxx")

let () =
  match g () with
  | "xxxbar" -> ()
  | _ -> assert false
;;

let h = f Null

let () =
  match h () with
  | "foo" -> ()
  | _ -> assert false
;;

type 'a nref = { mutable v : 'a or_null }

let x : string nref = { v = Null }

let () =
  match x.v with
  | Null -> ()
  | _ -> assert false
;;

let () = x.v <- This "foo"

let () =
  match x.v with
  | This "foo" -> ()
  | _ -> assert false
;;

let () = x.v <- Null

let () =
  match x.v with
  | Null -> ()
  | _ -> assert false
;;
