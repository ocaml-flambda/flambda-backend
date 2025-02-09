(* TEST
 reference = "${test_source_directory}/atat_unboxed_gadts.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   flags = "-extension-universe no_extensions";
   compiler_reference = "${test_source_directory}/atat_unboxed_gadts_disabled.compilers.reference";
   ocamlc.byte;
   check-ocamlc.byte-output;
 } {
   flags = "-extension layouts_alpha";
   native;
 } {
   flags = "-extension layouts_alpha -Oclassic";
   native;
 } {
   flags = "-extension layouts_alpha -O3";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }{
   flags = "";
   bytecode;
 }{
   flags = "";
   native;
 }
*)

(***************)
(* Basic tests *)

type ('a : value & value) t =
  | I : #(int * int) -> #(int * int) t [@@unboxed]

let print_t (type a : value & value) prefix (I #(x, y) : a t) =
  Printf.printf "%s: I #(%d, %d) \n" prefix x y

let apply_twice f (x : _ t) = f (f x)

let test () =
  let int_gadt = I #(1, 2) in
  let zero_snd (type a : value & value) (I #(x, _) : a t) : a t =
    I #(x, 0)
  in
  let swap (type a : value & value) (I #(x, y) : a t) : a t =
    I #(y, x)
  in
  let[@inline never] add
      (type a : value & value) (I #(a, b) : a t) (I #(c, d)) : a t =
    I #(a+c, b+d)
  in
  let[@inline never] add_int_gadt t = add t int_gadt in
  print_t "int_gadt" int_gadt;
  print_t "Test 1, zero_snd int_gadt" (zero_snd int_gadt);
  print_t "Test 2, swap int_gadt" (swap int_gadt);
  print_t "Test 3, swap (zero_snd int_gadt)" (swap (zero_snd int_gadt));
  print_t "Test 4, apply_twice swap int_gadt" (apply_twice swap int_gadt);
  print_t "Test 5, add_int_gadt (zero_snd int_gadt)"
    (add_int_gadt (zero_snd int_gadt));
  print_newline ()

let () = test ()

(****************************)
(* Result "unboxed variant" *)

module Result = struct
  type ('a, 'b) t = Ok of 'a | Error of 'b
end

module Result_u : sig
  type ('a, 'b) t : immediate & value

  val to_result : ('a, 'b) t -> ('a, 'b) Result.t
  val of_result : ('a, 'b) Result.t -> ('a, 'b) t
end = struct
  type ('a, 'b, 'c) tag =
    | Ok : ('a, 'b, 'a) tag
    | Error : ('a, 'b, 'b) tag

  type ('a, 'b) t =
    | T : #(('a, 'b, 'c) tag * 'c) -> ('a, 'b) t [@@unboxed]

  let to_result (type a) (type b) (T #(tag, x) : (a, b) t) : (a, b) Result.t =
    match tag with
    | Ok -> Ok x
    | Error -> Error x

  let of_result = function
    | Result.Ok x -> T #(Ok, x)
    | Result.Error x -> T #(Error, x)
end

let print_result prefix (r : (int, string) Result_u.t) =
  let r = Result_u.to_result r in
  match r with
  | Result.Ok x -> Printf.printf "%s: Ok %d\n" prefix x
  | Result.Error s -> Printf.printf "%s: Error %s\n" prefix s

let test () =
  let ok_value = Result_u.of_result (Result.Ok 42) in
  let error_value = Result_u.of_result (Result.Error "hi") in

  (* Test passing values to functions *)
  let add_one r =
    match Result_u.to_result r with
    | Result.Ok x -> Result_u.of_result (Result.Ok (x + 1))
    | Result.Error _ as e -> Result_u.of_result e
  in

  (* Test returning values from functions *)
  let double r =
    match Result_u.to_result r with
    | Result.Ok x -> Result_u.of_result (Result.Ok (x * 2))
    | Result.Error _ as e -> Result_u.of_result e
  in

  (* Test closures *)
  let[@inline never] rec add_n n =
    if n = 0 then
      fun x -> x
    else
      fun x -> add_one (add_n (n - 1) x)
  in

  (* Test higher-order functions *)
  let apply_twice f r = f (f r) in

  print_result "ok_value" ok_value;
  print_result "error_value" error_value;

  print_result "Test 6, add_one" (add_one ok_value);
  print_result "Test 7, double" (double ok_value);
  print_result "Test 8, apply_twice add_one" (apply_twice add_one ok_value);
  print_result "Test 9, apply_twice double" (apply_twice double ok_value);

  print_result "Test 10, add_n 0" (add_n 0 ok_value);
  print_result "Test 11, add_n 100" (add_n 100 ok_value);
  print_result "Test 12, double error_value (no-op)" (double error_value);
  print_newline ()

let _ = test ()

(************************)
(* Nested unboxed GADTS *)

type nothing = |

module Result_u_VV : sig
  type ('a : value & value, 'b : value & value) t : immediate & (value & value)
  val ok_exn : ('a, 'b) t -> 'a
  val error_exn : ('a, 'b) t -> 'b
  val ok : 'a -> ('a, _) t
  val error : 'b -> (_, 'b) t
  val is_ok : (_, _) t -> bool
end = struct
  type ('a : (value & value), 'b : (value & value), 'c : (value & value)) tag =
    | Ok : ('a, 'b, 'a) tag
    | Error : ('a, 'b, 'b) tag

  type ('a : value & value, 'b : value & value) t =
    | T : #(('a, 'b, 'c) tag * 'c) -> ('a, 'b) t [@@unboxed]

  let is_ok (type a : value & value) (type b : value & value)
        (T #(tag, x) : (a, b) t) : bool =
    match tag with Ok -> true | Error -> false

  let ok_exn (type a : value & value) (type b : value & value)
        (T #(tag, x) : (a, b) t) : a =
    match tag with
    | Ok -> x
    | Error -> match invalid_arg "" with (_ : nothing) -> .

  let error_exn (type a : value & value) (type b : value & value)
        (T #(tag, x) : (a, b) t) : b =
    match tag with
    | Error -> x
    | Ok -> match invalid_arg "" with (_ : nothing) -> .

  let ok x = T #(Ok, x)
  let error x = T #(Error, x)
end

let print_result_vv prefix (r : (#(float * unit), #(int * string)) Result_u_VV.t) =
  if Result_u_VV.is_ok r then
    let #(f, u) = Result_u_VV.ok_exn r in
    Printf.printf "%s: Ok #(%.2f, _)\n" prefix f
  else
    let #(i, s) = Result_u_VV.error_exn r in
    Printf.printf "%s: Error #(%d, %s)\n" prefix i s

let test () =
  (* Create values for nested GADTs *)
  let ok_value = Result_u_VV.ok (#(3.14, ())) in
  let error_value = Result_u_VV.error (#(42, "foo")) in

  (* Test passing values to functions *)
  let increment_float r =
    if Result_u_VV.is_ok r then
      let #(f, u) = Result_u_VV.ok_exn r in
      Result_u_VV.ok (#(f +. 1.0, u))
    else
      r
  in

  let append_error r =
    if Result_u_VV.is_ok r then
      r
    else
      let #(i, s) = Result_u_VV.error_exn r in
      Result_u_VV.error (#(i, s ^ "bar"))
  in

  (* Test returning values from functions *)
  let double_int r =
    if Result_u_VV.is_ok r then
      r
    else
      let #(i, s) = Result_u_VV.error_exn r in
      Result_u_VV.error (#(i * 2, s))
  in

  (* Test closures *)
  let[@inline never] rec add_n n =
    if n = 0 then
      fun x -> x
    else
      fun x -> increment_float (add_n (n - 1) x)
  in

  (* Test higher-order functions *)
  let apply_twice f r = f (f r) in

  let assert_invalid_arg f =
    match f () with
    | exception Invalid_argument _ -> "pass"
    | _ -> assert false
  in

  print_result_vv "ok_value" ok_value;
  print_result_vv "error_value" error_value;
  print_result_vv "Test 13, increment_float" (increment_float ok_value);
  print_result_vv "Test 14, append_error" (append_error error_value);
  print_result_vv "Test 15, double_int" (double_int error_value);
  print_result_vv "Test 16, apply_twice increment_float"
    (apply_twice increment_float ok_value);
  print_result_vv "Test 17, add_n 0" (add_n 0 ok_value);
  print_result_vv "Test 18, add_n 100" (add_n 100 ok_value);
  print_result_vv "Test 19, add_n 0 to error_value (no-op)"
    (add_n 0 error_value);
  print_result_vv "Test 20, add_n 100 to error_value (no-op)"
    (add_n 100 error_value);
  print_result_vv "Test 21, append_error to ok_value (no-op)"
    (append_error ok_value);
  Printf.printf "Test 22, (ok_exn error_value) raises: %s\n"
    (assert_invalid_arg (fun () -> Result_u_VV.ok_exn error_value));
  Printf.printf "Test 23, (error_exn (append_error ok_value)) raises: %s\n"
    (assert_invalid_arg
       (fun () -> Result_u_VV.error_exn (append_error ok_value)));
  print_newline ()

let () = test ()
