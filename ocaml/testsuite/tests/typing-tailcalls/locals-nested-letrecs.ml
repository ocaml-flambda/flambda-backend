(* TEST
  flags = "-no-always-tco";
  expect;
*)

(* This test is copied 8 times, all combinations of
   parent = [struct let; expression let]
   let local_ str in [outer1; outer2; inner1; inner2]

   In each body of {outer,inner}{1,2}, there is a potential error where the
   function passes in a local to a function application in tail position that
   is inferred as a tail call. Depending on the specific combination, this
   might generate a type error. We can only collect one type error at a time,
   so we copy the test one time per possible error.


(* Template that does not type error. The copies of the test duplicate
copy the template, but place a local_ annotation in a different place. *)

let rec outer1 (local_ _) =
  let str = "outer1" in outer2 str

and outer2 (local_ _) =
  let rec inner1 (local_ _) =
    let str = "inner1" in inner2 str
  and inner2 (local_ _) =
    if Sys.opaque_identity true
    then "goodbye"
    else begin
      let str = "inner2" in outer1 str
    end
  in
  let str = "outer2" in inner1 str

[%%expect {|
val outer1 : local_ string -> string = <fun>
val outer2 : local_ string -> string = <fun>
|}]

*)


(* Copy 1/8: (parent struct let, outer1).
   See the comment above ("Nested letrec") before modifying. *)
let rec outer1 (local_ _) =
  let local_ str = "outer1" in outer2 str

and outer2 (local_ _) =
  let rec inner1 (local_ _) =
    let str = "inner1" in inner2 str
  and inner2 (local_ _) =
    if Sys.opaque_identity true
    then "goodbye"
    else begin
      let str = "inner2" in outer1 str
    end
  in
  let str = "outer2" in inner1 str

[%%expect {|
Line 2, characters 38-41:
2 |   let local_ str = "outer1" in outer2 str
                                          ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

(* Copy 2/8: (parent struct let, outer2).
   See the comment above ("Nested letrec") before modifying. *)
let rec outer1 (local_ _) =
  let str = "outer1" in outer2 str

and outer2 (local_ _) =
  let rec inner1 (local_ _) =
    let str = "inner1" in inner2 str
  and inner2 (local_ _) =
    if Sys.opaque_identity true
    then "goodbye"
    else begin
      let str = "inner2" in outer1 str
    end
  in
  let local_ str = "outer2" in inner1 str

[%%expect {|
val outer1 : local_ string -> string = <fun>
val outer2 : local_ string -> string = <fun>
|}]

(* Copy 3/8: (parent struct let, inner1).
   See the comment above ("Nested letrec") before modifying. *)
let rec outer1 (local_ _) =
  let str = "outer1" in outer2 str

and outer2 (local_ _) =
  let rec inner1 (local_ _) =
    let local_ str = "inner1" in inner2 str
  and inner2 (local_ _) =
    if Sys.opaque_identity true
    then "goodbye"
    else begin
      let str = "inner2" in outer1 str
    end
  in
  let str = "outer2" in inner1 str

[%%expect {|
Line 6, characters 40-43:
6 |     let local_ str = "inner1" in inner2 str
                                            ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

(* Copy 4/8: (parent struct let, inner2).
   See the comment above ("Nested letrec") before modifying. *)
let rec outer1 (local_ _) =
  let str = "outer1" in outer2 str

and outer2 (local_ _) =
  let rec inner1 (local_ _) =
    let str = "inner1" in inner2 str
  and inner2 (local_ _) =
    if Sys.opaque_identity true
    then "goodbye"
    else begin
      let local_ str = "inner2" in outer1 str
    end
  in
  let str = "outer2" in inner1 str

[%%expect {|
Line 11, characters 42-45:
11 |       let local_ str = "inner2" in outer1 str
                                               ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

(* Copy 5/8: (parent expression let, outer1).
   See the comment above ("Nested letrec") before modifying. *)
let () =
  let rec outer1 (local_ _) =
    let local_ str = "outer1" in outer2 str

  and outer2 (local_ _) =
    let rec inner1 (local_ _) =
      let str = "inner1" in inner2 str
    and inner2 (local_ _) =
      if Sys.opaque_identity true
      then "goodbye"
      else begin
        let str = "inner2" in outer1 str
      end
    in
    let str = "outer2" in inner1 str
  in ignore outer1

[%%expect {|
Line 3, characters 40-43:
3 |     let local_ str = "outer1" in outer2 str
                                            ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

(* Copy 6/8: (parent expression let, outer2).
   See the comment above ("Nested letrec") before modifying. *)
let () =
  let rec outer1 (local_ _) =
    let str = "outer1" in outer2 str

  and outer2 (local_ _) =
    let rec inner1 (local_ _) =
      let str = "inner1" in inner2 str
    and inner2 (local_ _) =
      if Sys.opaque_identity true
      then "goodbye"
      else begin
        let str = "inner2" in outer1 str
      end
    in
    let local_ str = "outer2" in inner1 str
  in ignore outer1

[%%expect {|
|}]

(* Copy 7/8: (parent expression let, inner1).
   See the comment above ("Nested letrec") before modifying. *)
let () =
  let rec outer1 (local_ _) =
    let str = "outer1" in outer2 str

  and outer2 (local_ _) =
    let rec inner1 (local_ _) =
      let local_ str = "inner1" in inner2 str
    and inner2 (local_ _) =
      if Sys.opaque_identity true
      then "goodbye"
      else begin
        let str = "inner2" in outer1 str
      end
    in
    let str = "outer2" in inner1 str
  in ignore outer1

[%%expect {|
Line 7, characters 42-45:
7 |       let local_ str = "inner1" in inner2 str
                                              ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

(* Copy 8/8: (parent expression let, inner2).
   See the comment above ("Nested letrec") before modifying. *)
let () =
  let rec outer1 (local_ _) =
    let str = "outer1" in outer2 str

  and outer2 (local_ _) =
    let rec inner1 (local_ _) =
      let str = "inner1" in inner2 str
    and inner2 (local_ _) =
      if Sys.opaque_identity true
      then "goodbye"
      else begin
        let local_ str = "inner2" in outer1 str
      end
    in
    let str = "outer2" in inner1 str
  in ignore outer1

[%%expect {|
Line 12, characters 44-47:
12 |         let local_ str = "inner2" in outer1 str
                                                 ^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]
