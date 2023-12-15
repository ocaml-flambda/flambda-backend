(* TEST
   flags = "-extension layouts_alpha"
*)

module Int64_u = Stdlib__Int64_u
module Int32_u = Stdlib__Int32_u
module Nativeint_u = Stdlib__Nativeint_u

(* Print all individual successful tests; used for debugging, as it will cause
   this test to fail *)
let debug_tests = false

(* Constant seed for repeatable random-testing properties *)
(* I hear primes are often good?  This is an 11-digit prime in the decimal
   expansion of [e], containing the earliest 10-digit prime the decimal
   expansion of [e] per OEIS sequence A098963. *)
let () = Random.init 61000000007

let to_ocaml_string s = "\"" ^ String.escaped s ^ "\""

type 'a result = {
  expected : 'a;
  actual : 'a;
  equal : 'a -> 'a -> bool;
  to_string : 'a -> string
}

module type Result = sig
  type t
  val equal : t -> t -> bool
  val to_string : t -> string
end

let mk_result' equal to_string = fun ~expected ~actual ->
  { expected; actual; equal; to_string }

let mk_result (type a) (module M : Result with type t = a) =
  mk_result' M.equal M.to_string

let float_result     = mk_result (module Float)
let bool_result      = mk_result (module Bool)
let int_result       = mk_result (module Int)
let int32_result     = mk_result (module Int32)
let int64_result     = mk_result (module Int64)
let nativeint_result = mk_result (module Nativeint)
let string_result    = mk_result' String.equal to_ocaml_string

let option_result (type a) (module M : Result with type t = a)  =
  mk_result'
    (Option.equal M.equal)
    (function
      | None   -> "None"
      | Some x -> "Some (" ^ M.to_string x ^ ")")

type 'a generator =
  | Rand  of (unit -> 'a)
  | Const of 'a

let map_generator f = function
  | Rand  r -> Rand (fun () -> f (r ()))
  | Const c -> Const (f c)

type 'a input = {
  generators : 'a generator list;
  to_string  : 'a -> string
}

module type Integer = sig
  type t
  (* Interesting constants *)
  val zero : t
  val one : t
  val minus_one : t
  val max_int : t
  val min_int : t
  (* String generation *)
  val to_string : t -> string
  (* Comparison (for zero-testing) *)
  val equal : t -> t -> bool
  (* Arithmetic (for generating small numbers) *)
  val sub : t -> t -> t
  val shift_left : t -> int -> t
end

let one_thousand (type a) (module I : Integer with type t = a) =
  let open I in
  let i1024 = shift_left one 10 in
  let i16   = shift_left one 4  in
  let i8    = shift_left one 3  in
  sub (sub i1024 i16) i8

let two_thousand (type a) (module I : Integer with type t = a) =
  I.shift_left (one_thousand (module I)) 1

let unit_input =
  { generators = [Const ()]
  ; to_string = Unit.to_string
  }

let bool_input =
  { generators = [Const false; Const true]
  ; to_string = Bool.to_string
  }

let float_input =
  { generators = [ Const 0.
                 ; Const 1.
                 ; Const (-1.)
                 ; Const Float.max_float
                 ; Const Float.min_float
                 ; Const Float.epsilon
                 ; Const Float.nan
                 ; Const Float.infinity
                 ; Const Float.neg_infinity
                 ; Rand (fun () -> Random.float 2000. -. 1000.)
                 ; Rand (fun () -> Int64.float_of_bits (Random.bits64 ()))
                 ]
  ; to_string = Float.to_string
  }

let integer_input
      (type a) (module I : Integer with type t = a)
      rand_range rand_full =
  let rand_small () =
    let i0_to_2000 = rand_range (two_thousand (module I)) in
    I.sub i0_to_2000 (one_thousand (module I))
  in
  { generators = [ Const I.zero
                 ; Const I.one
                 ; Const I.minus_one
                 ; Const I.max_int
                 ; Const I.min_int
                 ; Rand  rand_small
                 ; Rand  rand_full
                 ]
  ; to_string = I.to_string
  }

let nonzero_integer_input
      (type a) (module I : Integer with type t = a)
      rand_range rand_full =
  let { generators; to_string } =
    integer_input (module I) rand_range rand_full
  in
  let generators =
    generators |>
    List.filter_map
      (function
        | Const c ->
            if I.equal c I.zero
            then None
            else Some (Const c)
        | Rand  r ->
            Some (Rand (fun () ->
              let n = ref I.zero in
              while I.equal !n I.zero do
                n := r ()
              done;
              !n)))
  in
  { generators; to_string }

let int_input = integer_input (module Int) Random.int Random.bits
let int32_input = integer_input (module Int32) Random.int32 Random.bits32
let int64_input = integer_input (module Int64) Random.int64 Random.bits64
let nativeint_input =
  integer_input (module Nativeint) Random.nativeint Random.nativebits
let nonzero_int64_input =
  nonzero_integer_input (module Int64) Random.int64 Random.bits64

let int64_shift_amount_input =
  { generators = List.init 64 (fun c -> Const c)
  ; to_string  = Int.to_string
  }

let int64_string_input =
  { generators = List.map
                   (map_generator Int64.to_string)
                   int64_input.generators
  ; to_string  = to_ocaml_string
  }

let product2 ~f xs ys =
  List.concat_map (fun x ->
    List.map (fun y ->
        f x y)
      ys)
    xs

let two_inputs in1 in2 =
  { generators = product2 in1.generators in2.generators ~f:(fun gen1 gen2 ->
      match gen1, gen2 with
      | Const c1, Const c2 -> Const (c1, c2)
      | Const c1, Rand  r2 -> Rand (fun () -> c1, r2 ())
      | Rand  r1, Const c2 -> Rand (fun () -> r1 (), c2)
      | Rand  r1, Rand  r2 -> Rand (fun () -> r1 (), r2 ())
    )
  ; to_string = fun (x1, x2) ->
      Printf.sprintf "(%s, %s)" (in1.to_string x1) (in2.to_string x2)
  }

let passed { actual; expected; equal; _ } = equal actual expected

let test ?(n=100) name prop { generators; to_string = input_to_string } =
  let test input =
    let {expected; actual; to_string} as result = prop input in
    let print_test outcome =
      Printf.printf "Test %s: %s. Input = %s; expected = %s; actual = %s\n"
        outcome name
        (input_to_string input) (to_string expected) (to_string actual)
    in
    if passed result then begin
      if debug_tests then print_test "succeeded"
    end
    else
      print_test "failed"
  in
  List.iter
    (function
      | Const c -> test c
      | Rand  r -> for _ = 1 to n do test (r ()) done)
    generators

let test_same
      ~input ~result ~apply_expected ~apply_actual
      ?n name expected actual =
  test ?n name
    (fun x ->
       result
         ~expected:(apply_expected expected x)
         ~actual:(apply_actual actual x))
    input

let test_constant ?n name expected actual result =
  test ?n name (fun () -> result ~expected ~actual) unit_input

let test_same_unary ?n name input result expected actual =
  test_same
    ~input
    ~result
    ~apply_expected:Fun.id
    ~apply_actual:Fun.id
    ?n name expected actual

let test_same_binary ?n name input1 input2 result expected actual =
  test_same
    ~input:(two_inputs input1 input2)
    ~result
    ~apply_expected:(fun f (x,y) -> f x y)
    ~apply_actual:(fun f (x,y) -> f x y)
    ?n name expected actual

let test_unary ?n name f fu =
  test_same_unary ?n name int64_input int64_result f
    (fun x -> Int64_u.to_int64 (fu (Int64_u.of_int64 x)))

let test_unary_of ?n name f fu result =
  test_same_unary ?n name int64_input result f
    (fun x -> fu (Int64_u.of_int64 x))

let test_unary_to ?n name f fu input =
  test_same_unary ?n name input int64_result f
    (fun x -> Int64_u.to_int64 (fu x))

let test_binary' ~second_input ?n name f fu =
  test_same_binary ?n name int64_input second_input int64_result f
    (fun x y -> Int64_u.to_int64
                  (fu
                     (Int64_u.of_int64 x)
                     (Int64_u.of_int64 y)))

let test_binary = test_binary' ~second_input:int64_input

let test_division = test_binary' ~second_input:nonzero_int64_input

let test_binary_of ?n name f fu result =
  test_same_binary ?n name int64_input int64_input result f
    (fun x y -> fu
                  (Int64_u.of_int64 x)
                  (Int64_u.of_int64 y))

let test_shift ?n name shift shiftu =
  test_same_binary
    ?n name int64_input int64_shift_amount_input int64_result shift
    (fun x y -> Int64_u.to_int64
                  (shiftu
                     (Int64_u.of_int64 x)
                     y))

let int64_u_of_int32 x = Int64_u.of_int32_u (Int32_u.of_int32 x)
let int64_u_to_int32 x = Int32_u.to_int32 (Int64_u.to_int32_u x)
let int64_u_of_nativeint x = Int64_u.of_nativeint_u (Nativeint_u.of_nativeint x)
let int64_u_to_nativeint x = Nativeint_u.to_nativeint (Int64_u.to_nativeint_u x)

let () =
  test_unary     "neg"                 Int64.neg                 Int64_u.neg;
  test_binary    "add"                 Int64.add                 Int64_u.add;
  test_binary    "sub"                 Int64.sub                 Int64_u.sub;
  test_binary    "mul"                 Int64.mul                 Int64_u.mul;
  test_division  "div"                 Int64.div                 Int64_u.div;
  test_division  "unsigned_div"        Int64.unsigned_div        Int64_u.unsigned_div;
  test_division  "rem"                 Int64.rem                 Int64_u.rem;
  test_division  "unsigned_rem"        Int64.unsigned_rem        Int64_u.unsigned_rem;
  test_unary     "succ"                Int64.succ                Int64_u.succ;
  test_unary     "pred"                Int64.pred                Int64_u.pred;
  test_unary     "abs"                 Int64.abs                 Int64_u.abs;
  test_binary    "logand"              Int64.logand              Int64_u.logand;
  test_binary    "logor"               Int64.logor               Int64_u.logor;
  test_binary    "logxor"              Int64.logxor              Int64_u.logxor;
  test_unary     "lognot"              Int64.lognot              Int64_u.lognot;
  test_shift     "shift_left"          Int64.shift_left          Int64_u.shift_left;
  test_shift     "shift_right"         Int64.shift_right         Int64_u.shift_right;
  test_shift     "shift_right_logical" Int64.shift_right_logical Int64_u.shift_right_logical;
  test_unary_to  "of_int"              Int64.of_int              Int64_u.of_int               int_input;
  test_unary_of  "to_int"              Int64.to_int              Int64_u.to_int               int_result;
  test_unary_of  "unsigned_to_int"     Int64.unsigned_to_int     Int64_u.unsigned_to_int      (option_result (module Int));
  test_unary_to  "of_float"            Int64.of_float            Int64_u.of_float             float_input;
  test_unary_of  "to_float"            Int64.to_float            Int64_u.to_float             float_result;
  test_unary_to  "of_int32"            Int64.of_int32            Int64_u.of_int32             int32_input;
  test_unary_of  "to_int32"            Int64.to_int32            Int64_u.to_int32             int32_result;
  test_unary_to  "of_nativeint"        Int64.of_nativeint        Int64_u.of_nativeint         nativeint_input;
  test_unary_of  "to_nativeint"        Int64.to_nativeint        Int64_u.to_nativeint         nativeint_result;
  test_unary_to  "of_int32_u"          Int64.of_int32            int64_u_of_int32             int32_input;
  test_unary_of  "to_int32_u"          Int64.to_int32            int64_u_to_int32             int32_result;
  test_unary_to  "of_nativeint_u"      Int64.of_nativeint        int64_u_of_nativeint         nativeint_input;
  test_unary_of  "to_nativeint_u"      Int64.to_nativeint        int64_u_to_nativeint         nativeint_result;
  test_unary_to  "of_string"           Int64.of_string           Int64_u.of_string            int64_string_input;
  test_unary_of  "to_string"           Int64.to_string           Int64_u.to_string            string_result;
  test_unary_to  "bits_of_float"       Int64.bits_of_float       Int64_u.bits_of_float        float_input;
  test_unary_of  "float_of_bits"       Int64.float_of_bits       Int64_u.float_of_bits        float_result;
  test_binary_of "compare"             Int64.compare             Int64_u.compare              int_result;
  test_binary_of "unsigned_compare"    Int64.unsigned_compare    Int64_u.unsigned_compare     int_result;
  test_binary_of "equal"               Int64.equal               Int64_u.equal                bool_result;
  test_binary    "min"                 Int64.min                 Int64_u.min;
  test_binary    "max"                 Int64.max                 Int64_u.max;
  ()
