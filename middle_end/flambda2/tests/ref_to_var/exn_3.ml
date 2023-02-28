exception Saucisse

let id = Sys.opaque_identity

let g () = () [@@inline never]

let f x tt b =
  let r = ref x in
  let v =
    try
      while id false do
        ()
      done;
      while id false do
        let _ = id !r in
        (* if id false then raise Saucisse; *)
        g ();
        if b then r := 12;
        ()
      done;
      !r
    with Saucisse -> !r + 1
  in
  v * !r
  [@@inlined always]

let zozo z b =
  let uu w pp = f w pp b in
  let a, b = if Sys.opaque_identity false then 1, 1 else 2, 2 in
  let ouou x = Sys.opaque_identity x in
  let a = ouou a in
  let o = Sys.opaque_identity z in
  if Sys.opaque_identity false
  then
    let koko = Sys.opaque_identity a in
    uu o koko
  else
    let kuku = Sys.opaque_identity b in
    uu o kuku
  [@@inlined always]

let chose z =
  ignore z;
  zozo z false
