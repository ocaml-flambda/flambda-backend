type t = { mutable x : int }

let rec go b r =
  if Sys.opaque_identity false
  then r
  else if b > 0
  then go b { x = 0 }
  else (
    r.x <- r.x + 1;
    go b r)

let f () = ((go [@inlined]) 0 { x = 0 }).x
