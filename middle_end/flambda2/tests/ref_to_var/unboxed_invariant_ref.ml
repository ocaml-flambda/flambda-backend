type t = { mutable x : int }

let[@inline] f l =
  let t = { x = 0 } in
  List.iter (fun () -> t.x <- 1 + t.x) l;
  t

let test l = (f l).x
