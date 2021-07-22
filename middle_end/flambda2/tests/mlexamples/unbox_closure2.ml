
let foobar b x z =
  let f =
    if b then begin
      let[@inline] bar1 y = x + y in
      Ok bar1
    end else begin
      let[@inline] bar2 y = z * x * y in
      Error bar2
    end
  in
  match f with
  | Ok f -> f x
  | Error f -> f x


