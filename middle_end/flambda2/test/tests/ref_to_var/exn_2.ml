exception Saucisse

let id = Sys.opaque_identity

let g () = () [@@inline never]

let f x tt =
  let r = ref x in
  let v =
    try
      while id false do
        let _ = id !r in
        (* if id false then raise Saucisse; *)
        g ();
        ()
      done;
      !r
    with Saucisse -> !r + 1
  in
  v * !r
  [@@inlined always]

let zozo z =
  let uu w pp = (f [@inlined always]) w pp in
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
