(* TEST
   flags = "-dlambda -dno-locations -dno-unique-ids";
   expect;
*)

(* Basic usage: redefine atomics. *)

module Basic = struct
  type 'a atomic = { mutable filler: unit; mutable x : 'a [@atomic] }

  let get (type a) (r : a atomic) : a = r.x

  let set (type a) (r : a atomic) (v : a) : unit = r.x <- v

  let cas (type a) (r : a atomic) oldv newv =
    Atomic.Loc.compare_and_set [%atomic.loc r.x] oldv newv

  let[@inline never] get_loc (type a) (r : a atomic) : a Atomic.Loc.t =
    [%atomic.loc r.x]

  let slow_cas (type a) (r : a atomic) oldv newv =
    Atomic.Loc.compare_and_set (get_loc r) oldv newv
end
[%%expect{|
Line 9, characters 4-30:
9 |     Atomic.Loc.compare_and_set [%atomic.loc r.x] oldv newv
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Atomic.Loc"
|}];;


(* Atomic fields must be mutable. *)
module Error1 = struct
  type t = { x : int [@atomic] }
end
[%%expect{|
Line 2, characters 13-30:
2 |   type t = { x : int [@atomic] }
                 ^^^^^^^^^^^^^^^^^
Error: The label "x" must be mutable to be declared atomic.
|}];;


(* [%atomic.loc _] payload must be a record field access *)
module Error2 = struct
  type t = { mutable x : int [@atomic] }
  let f t = [%atomic.loc t]
end
[%%expect{|
Line 3, characters 12-27:
3 |   let f t = [%atomic.loc t]
                ^^^^^^^^^^^^^^^
Error: Invalid "[%atomic.loc]" payload, a record field access is expected.
|}];;


(* [%atomic.loc _] only works on atomic fields *)
module Error3 = struct
  type t = { x : int }
  let f t = [%atomic.loc t.x]
end
[%%expect{|
Line 3, characters 12-29:
3 |   let f t = [%atomic.loc t.x]
                ^^^^^^^^^^^^^^^^^
Error: The record field "x" is not atomic
|}];;


(* Check module interface checking: it is not allowed to remove or add
   atomic attributes. *)

module Wrong1 = (struct
  type t = { mutable x : int }
end : sig
  (* adding an 'atomic' attribute missing in the implementation: invalid. *)
  type t = { mutable x : int [@atomic] }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int; } end
       is not included in
         sig type t = { mutable x : int [@atomic]; } end
       Type declarations do not match:
         type t = { mutable x : int; }
       is not included in
         type t = { mutable x : int [@atomic]; }
       Fields do not match:
         "mutable x : int;"
       is not the same as:
         "mutable x : int [@atomic];"
       The second is atomic and the first is not.
|}];;

module Wrong2 = (struct
  type t = { mutable x : int [@atomic] }
end : sig
  (* removing an 'atomic' attribute present in the implementation: invalid. *)
  type t = { mutable x : int }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int [@atomic] }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int [@atomic]; } end
       is not included in
         sig type t = { mutable x : int; } end
       Type declarations do not match:
         type t = { mutable x : int [@atomic]; }
       is not included in
         type t = { mutable x : int; }
       Fields do not match:
         "mutable x : int [@atomic];"
       is not the same as:
         "mutable x : int;"
       The first is atomic and the second is not.
|}];;

module Ok = (struct
  type t = { mutable x : int [@atomic] }
end : sig
  type t = { mutable x : int [@atomic] }
end)
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Ok/353" (makeblock 0))
module Ok : sig type t = { mutable x : int [@atomic]; } end
|}];;



(* Inline records are supported, including in extensions. *)

module Inline_record = struct
  type t = A of { mutable x : int [@atomic] }

  let test : t -> int = fun (A r) -> r.x
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Inline_record/361"
  (let (test = (function {nlocal = 0} param : int (atomic_load_ptr param 0)))
    (makeblock 0 test)))
>> Fatal error: Blambda_of_lambda: atomic_load_ptr takes exactly 1 argument
Uncaught exception: Misc.Fatal_error

|}];;

module Extension_with_inline_record = struct
  type t = ..
  type t += A of { mutable x : int [@atomic] }

  (* one should see in the -dlambda output below that the field offset is not 0
     as one could expect, but 1, due to an extra argument in extensible variants. *)
  let test : t -> int = function
    | A r -> r.x
    | _ -> 0

  let () = assert (test (A { x = 42 }) = 42)
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Extension_with_inline_record/369"
  (let
    (A =
       (makeblock_unique 248 "Extension_with_inline_record.A"
         (caml_fresh_oo_id 0))
     test =
       (function {nlocal = 0} param : int
         (if (== (field_imm 0 param) A) (atomic_load_ptr param 0) 0))
     *match* =[int]
       (if (== (apply test (makemutable 0 (*,int) A 42)) 42) 0
         (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 11 11]))))
    (makeblock 0 A test)))
>> Fatal error: Blambda_of_lambda: atomic_load_ptr takes exactly 1 argument
Uncaught exception: Misc.Fatal_error

|}];;


(* Marking a field [@atomic] in a float-only record disables the unboxing optimization. *)
module Float_records = struct
  type t = { x : float; mutable y : float [@atomic] }

  (* one should see in the -dlambda output below that this creates a block of tag 0. *)
  let mk_t x y : t = { x; y }
  let get v = v.y
end
[%%expect{|
>> Fatal error: Translcore.transl_atomic_loc: atomic field in float record
Uncaught exception: Misc.Fatal_error

|}];;


(* Pattern-matching on atomic record fields is disallowed. *)
module Pattern_matching = struct
  type t = { x : int; mutable y : int [@atomic] }

  let forbidden { x; y } = x + y
end
[%%expect{|
Line 4, characters 16-24:
4 |   let forbidden { x; y } = x + y
                    ^^^^^^^^
Error: Atomic fields (here "y") are forbidden in patterns,
       as it is difficult to reason about when the atomic read
       will happen during pattern matching: the field may be read
       zero, one or several times depending on the patterns around it.
|}]

(* ... except for wildcards, to allow exhaustive record patterns. *)
module Pattern_matching_wildcard = struct
  type t = { x : int; mutable y : int [@atomic] }

  [@@@warning "+missing-record-field-pattern"]
  let warning { x } = x

  let allowed { x; y = _ } = x
end
[%%expect{|
Line 5, characters 14-19:
5 |   let warning { x } = x
                  ^^^^^
Warning 9 [missing-record-field-pattern]: the following labels are not bound in this record pattern:
y
Either bind these labels explicitly or add '; _' to the pattern.
(apply (field_imm 1 (global Toploop!)) "Pattern_matching_wildcard/410"
  (let
    (warning = (function {nlocal = 0} param : int (field_int 0 param))
     allowed = (function {nlocal = 0} param : int (field_int 0 param)))
    (makeblock 0 warning allowed)))

module Pattern_matching_wildcard :
  sig
    type t = { x : int; mutable y : int [@atomic]; }
    val warning : t -> int
    val allowed : t -> int
  end
|}]
