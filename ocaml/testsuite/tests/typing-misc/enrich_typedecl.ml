(* TEST
 expect;
*)

module rec A : sig
  type t = int * string
end = struct
  type t = A | B

  let f (x : t) =
    match x with
    | A -> ()
    | B -> ()
end;;
[%%expect{|
Lines 3-10, characters 6-3:
 3 | ......struct
 4 |   type t = A | B
 5 |
 6 |   let f (x : t) =
 7 |     match x with
 8 |     | A -> ()
 9 |     | B -> ()
10 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = A.t = A | B val f : t -> unit end
       is not included in
         sig type t = int * string end
       Type declarations do not match:
         type t = A.t = A | B
       is not included in
         type t = int * string
       The type A.t is not equal to the type int * string
|}]

module rec B : sig
  type 'a t = 'a
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Lines 3-10, characters 6-3:
 3 | ......struct
 4 |   type 'a t = A of 'a | B
 5 |
 6 |   let f (x : _ t) =
 7 |     match x with
 8 |     | A _ -> ()
 9 |     | B -> ()
10 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a B.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type ('a : any) t = 'a end
       Type declarations do not match:
         type 'a t = 'a B.t = A of 'a | B
       is not included in
         type ('a : any) t = 'a
       The type ('a : value) is not equal to the type ('a0 : any)
       because their layouts are different.
|}];;

module rec C : sig
  type 'a t = { x : 'a }
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Lines 3-10, characters 6-3:
 3 | ......struct
 4 |   type 'a t = A of 'a | B
 5 |
 6 |   let f (x : _ t) =
 7 |     match x with
 8 |     | A _ -> ()
 9 |     | B -> ()
10 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a C.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = { x : 'a; } end
       Type declarations do not match:
         type 'a t = 'a C.t = A of 'a | B
       is not included in
         type 'a t = { x : 'a; }
       The first is a variant, but the second is a record.
|}];;


module rec D : sig
  type 'a t = int
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Lines 3-10, characters 6-3:
 3 | ......struct
 4 |   type 'a t = A of 'a | B
 5 |
 6 |   let f (x : _ t) =
 7 |     match x with
 8 |     | A _ -> ()
 9 |     | B -> ()
10 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a D.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type ('a : any) t = int end
       Type declarations do not match:
         type 'a t = 'a D.t = A of 'a | B
       is not included in
         type ('a : any) t = int
       The type ('a : value) is not equal to the type ('a0 : any)
       because their layouts are different.
|}];;

module rec E : sig
  type 'a t = [> `Foo ] as 'a
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Lines 3-10, characters 6-3:
 3 | ......struct
 4 |   type 'a t = A of 'a | B
 5 |
 6 |   let f (x : _ t) =
 7 |     match x with
 8 |     | A _ -> ()
 9 |     | B -> ()
10 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a E.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `Foo ] end
       Type declarations do not match:
         type 'a t = 'a E.t = A of 'a | B
       is not included in
         type 'a t = 'a constraint 'a = [> `Foo ]
       The type 'a is not equal to the type [> `Foo ]
|}];;

module rec E2 : sig
  type 'a t = [ `Foo ]
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Lines 3-10, characters 6-3:
 3 | ......struct
 4 |   type 'a t = A of 'a | B
 5 |
 6 |   let f (x : _ t) =
 7 |     match x with
 8 |     | A _ -> ()
 9 |     | B -> ()
10 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a E2.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type ('a : any) t = [ `Foo ] end
       Type declarations do not match:
         type 'a t = 'a E2.t = A of 'a | B
       is not included in
         type ('a : any) t = [ `Foo ]
       The type ('a : value) is not equal to the type ('a0 : any)
       because their layouts are different.
|}];;

module rec E3 : sig
  type 'a t = [< `Foo ] as 'a
end = struct
  type 'a t = A of 'a | B

  let f (x : _ t) =
    match x with
    | A _ -> ()
    | B -> ()
end;;
[%%expect{|
Lines 3-10, characters 6-3:
 3 | ......struct
 4 |   type 'a t = A of 'a | B
 5 |
 6 |   let f (x : _ t) =
 7 |     match x with
 8 |     | A _ -> ()
 9 |     | B -> ()
10 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a E3.t = A of 'a | B val f : 'a t -> unit end
       is not included in
         sig type 'a t = 'a constraint 'a = [< `Foo ] end
       Type declarations do not match:
         type 'a t = 'a E3.t = A of 'a | B
       is not included in
         type 'a t = 'a constraint 'a = [< `Foo ]
       The type 'a is not equal to the type [< `Foo ]
|}];;


module rec F : sig
  type ('a, 'b) t = Foo of 'a
end = struct
  type ('a, 'b) t = Foo of 'b

  (* this function typechecks properly, which means that we've added the
     manisfest. *)
  let coerce : 'a 'b. ('a, 'b) t -> ('a, 'b) F.t = fun x -> x
end;;
[%%expect{|
Line 8, characters 60-61:
8 |   let coerce : 'a 'b. ('a, 'b) t -> ('a, 'b) F.t = fun x -> x
                                                                ^
Error: This expression has type ('a, 'b) t
       but an expression was expected of type ('a, 'b) F.t
|}];;
