exception Saucisse

let id = Sys.opaque_identity

let g () = () [@@inline never]

let f x =
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
