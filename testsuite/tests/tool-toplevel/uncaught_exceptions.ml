(* TEST
 expect;
*)

(* PR#8594 *)
Printexc.register_printer (fun e ->
  match e with
    | Division_by_zero -> Some "A division by zero is undefined"
    | _ -> None);;
[%%expect{|
Line 1, characters 0-25:
1 | Printexc.register_printer (fun e ->
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Alert unsafe_multidomain: Stdlib.Printexc.register_printer
Use [Printexc.Safe.register_printer].

- : unit = ()
|}];;

Printexc.register_printer (fun e ->
  match e with
    | Exit -> Some "Catching an exit"
    | _ -> None);;
[%%expect{|
Line 1, characters 0-25:
1 | Printexc.register_printer (fun e ->
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Alert unsafe_multidomain: Stdlib.Printexc.register_printer
Use [Printexc.Safe.register_printer].

- : unit = ()
|}];;

raise Not_found;;
[%%expect{|
Exception: Not_found.
|}];;

raise Exit;;
[%%expect{|
Exception: Catching an exit
|}];;

exception Foo of string;;
[%%expect {|
exception Foo of string
|}];;

raise (Foo "bar");;
[%%expect {|
Exception: Foo "bar".
|}];;

raise Division_by_zero;;
[%%expect {|
Exception: A division by zero is undefined
|}];;
