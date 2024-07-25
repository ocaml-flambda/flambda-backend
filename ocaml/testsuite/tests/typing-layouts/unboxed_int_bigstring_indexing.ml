(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha -O3";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)


module _ = struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let bigstring_of_string s =
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  let length = 300
  let reference_str = String.init length (fun i -> i * 7 mod 256 |> char_of_int)
  let create_bs () = reference_str |> bigstring_of_string
  let try_with f = try Ok (f ()) with err -> Error err


  external reference : bigstring -> int -> int
    = "%caml_bigstring_get16"

  external tested_s : bigstring -> int64# -> int
    = "%caml_bigstring_get16_indexed_by_int64#"

  external tested_u : bigstring -> int64# -> int
    = "%caml_bigstring_get16u_indexed_by_int64#"

  let of_boxed_index : int -> int64# = Stdlib_upstream_compatible.Int64_u.of_int
  let to_boxed_result : int -> int = fun x -> x
  let eq : int -> int -> bool = Int.equal

  let check_get_bounds, check_get =
    let bs_ref = create_bs () and bs_s = create_bs () and bs_u = create_bs () in
    let check_get_bounds i =
      match try_with (fun () -> tested_s bs_s i) with
      | Error (Invalid_argument _) -> ()
      | _ -> assert false
    in
    ( check_get_bounds
    , fun i ->
        let test_i = of_boxed_index i in
        match try_with (fun () -> reference bs_ref i) with
        | Ok res ->
          (match
             ( try_with (fun () -> tested_s bs_s test_i)
             , try_with (fun () -> tested_u bs_u test_i) )
           with
           | Ok s, Ok u ->
             assert (eq res (to_boxed_result s));
             assert (eq res (to_boxed_result u))
           | _ -> assert false)
        | Error (Invalid_argument _) -> check_get_bounds (of_boxed_index i)
        | Error _ ->
          (match try_with (fun () -> tested_s bs_s test_i) with
           | Error (Invalid_argument _) -> assert false
           | Error _ -> ()
           | Ok _ -> assert false) )
  ;;

  for i = -1 to length + 1 do
    check_get i
  done
  ;;


  check_get_bounds (-#9223372036854775808L);;
  check_get_bounds (-#9223372036854775807L);;
  check_get_bounds (#9223372036854775807L);;

end;;
