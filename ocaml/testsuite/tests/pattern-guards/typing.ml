(* TEST
   * expect *)

(* Test pattern guard typechecking. *)

(* Well-typed pattern guard with complex types. *)

let complex_types (x : int list option) : bool =
  let strs_opt = match x with
    | Some [ y ] when y + 1 > 5 -> Some ("foo", "bar")
    | Some ys when List.length ys match 0 -> Some ("baz", "qux")
    | Some _ | None -> None
  in
  match strs_opt with
    | Some strs when strs match ("foo", s2) -> String.equal s2 "bar"
    | Some _ | None -> false
;;
[%%expect {|
val complex_types : int list option -> bool = <fun>
|}];;

(* Ill-typed pattern in pattern guard. *)

let ill_typed_pattern (x : int list option) : bool =
  match x with
    | Some [ y ] when y match None -> true
    | _ -> false
[%%expect{|
Line 3, characters 30-34:
3 |     | Some [ y ] when y match None -> true
                                  ^^^^
Error: This pattern matches values of type 'a option
       but a pattern was expected which matches values of type int
|}]

(* Ill-typed usage of pattern guard-bound variables. *)

let ill_typed_pattern_var (x : int list option) : bool =
  match x with
    | Some xs when xs match [ y ] -> String.equal y "foo"
    | _ -> false
[%%expect{|
Line 3, characters 50-51:
3 |     | Some xs when xs match [ y ] -> String.equal y "foo"
                                                      ^
Error: This expression has type int but an expression was expected of type
         String.t = string
|}];;

(* Test rejection of pattern guards on mixed exception/value or-patterns *)

let reject_guarded_val_exn_orp k =
  match k () with
  | Some s | exception Failure s when s match "foo" -> s
  | _ -> "Not foo"
;;
[%%expect{|
Line 3, characters 4-32:
3 |   | Some s | exception Failure s when s match "foo" -> s
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Mixing value and exception patterns under when-guards is not supported.
|}];;
