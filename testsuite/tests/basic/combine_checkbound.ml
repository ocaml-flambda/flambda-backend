(* TEST
 native;
*)

let glob = ref (1, 2)
let[@inline never] combine1 x a =
  begin try
    glob := (2, x);
    glob := (3, a.(4));
  with
  | Invalid_argument _ -> ()
  end;
  !glob

let[@inline never] combine2 x a =
  let loc = ref (1, 2) in
  begin try
    loc := (2, x);
    loc := (3, a.(4));
  with
  | Invalid_argument _ -> ()
  end;
  !loc

let[@inline never] measure f =
  let empty_array = [| |] in
  let prebefore = Gc.minor_words () in
  let before = Gc.minor_words () in
  let r = f 42 empty_array in
  assert (r = (2, 42));
  let after = Gc.minor_words () in
  ((after -. before) -. (before -. prebefore))


let () =
  Printf.printf "%10s: %.0f\n" "combine1" (measure combine1);
  Printf.printf "%10s: %.0f\n" "combine2" (measure combine2)
