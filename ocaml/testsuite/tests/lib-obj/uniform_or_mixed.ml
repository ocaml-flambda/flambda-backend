(* TEST
  flags="-extension layouts_beta";
  native;
*)

type t_uniform1 = { x : int }
type t_uniform2 = floatarray
type t_uniform3 = int -> int

type t_mixed0 = { x : int64# }
type t_mixed1 = { x : string; y : int64# }
type t_mixed2 = { x : string; y : string; z : int64# }

let run (type a) test_name (obj : a) =
  let obj = Obj.repr obj in
  let uniform_or_mixed = Obj.Uniform_or_mixed.of_block obj in
  match Obj.Uniform_or_mixed.repr uniform_or_mixed with
  | Uniform ->
      assert (Obj.Uniform_or_mixed.is_uniform uniform_or_mixed);
      assert (not (Obj.Uniform_or_mixed.is_mixed uniform_or_mixed));
      Printf.printf "%s: uniform\n" test_name
  | Mixed { scannable_prefix_len } ->
      assert (Obj.Uniform_or_mixed.is_mixed uniform_or_mixed);
      assert (not (Obj.Uniform_or_mixed.is_uniform uniform_or_mixed));
      assert
        (Obj.Uniform_or_mixed.mixed_scannable_prefix_len_exn uniform_or_mixed =
            scannable_prefix_len);
      Printf.printf "%s: mixed (scannable_prefix_len = %d)\n"
        test_name scannable_prefix_len;
;;

let () = run "t_uniform1" ({ x = 3 } : t_uniform1)
let () = run "t_uniform2.0" (Float.Array.create 0 : t_uniform2)
let () = run "t_uniform2.1" (Float.Array.create 1 : t_uniform2)
let () = run "t_uniform3" ((fun x -> x) : t_uniform3)

let () = run "t_mixed0" ({ x = #4L } : t_mixed0)
let () = run "t_mixed1" ({ x = ""; y = #5L } : t_mixed1)
let () = run "t_mixed2" ({ x = ""; y = ""; z = #5L }: t_mixed2)

