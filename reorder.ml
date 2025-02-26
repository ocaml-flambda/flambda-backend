external int32_u_to_int32 : int32# -> (int32[@local_opt]) = "%box_int32" [@@warning "-187"]

external int32_u_of_int32 : (int32[@local_opt]) -> int32# = "%unbox_int32" [@@warning "-187"]

type t = {
  a : int32#;
  b : int;
}

let get_a t = t.a
let get_b t = t.b

let[@inline never][@local never] create () =
  let ra = Random.int 100 in
  let rb = Random.int 100 in
  Printf.eprintf "ra=%d rb=%d\n\n" ra rb;
  { a = int32_u_of_int32 (Int32.of_int ra);
    b = rb;
  }

let () =
  let r = create () in
  Printf.printf "a=%ld b=%d\n" (int32_u_to_int32 (get_a r)) (get_b r)
