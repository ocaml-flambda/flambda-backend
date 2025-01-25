(* TEST *)
let glob = ref []

let[@inline never] f g n =
  let a = local_ [n] in
  let b = [n+1] in
  let c = local_ [n+2] in
  glob := b;
  g a c;
  ()

type junk = { a : float; b : float; c : float; d : float; e : float; f : float }
let[@inline never] clear g n =
  let junk = local_ {a=n;b=n;c=n;d=n;e=n;f=n} in
  g junk;
  ()

(* As a header, this looks like tag 0 and marked,
   while as a value it's (probably) an invalid pointer *)
let ones = Int64.float_of_bits 0x3FFF_FF00_FFFF_FF00L
let () =
  for i = 1 to 1_000_000 do
    clear (fun _ -> ()) ones;
    f (fun _ _ -> ()) 42
  done;
  print_endline "ok"


external opaque_local : local_ 'a -> local_ 'a = "%opaque"

let[@inline never] g (local_ a) =
  let local_ z = (a, a) in
  Gc.minor ();
  let _ = opaque_local z in
  ()

let[@inline always] dead (local_ x) =
  let _ = opaque_local (x, x, x, x, x, x, x, x) in
  ()

let[@inline never] f (local_ x) =
  dead x;
  let r = (x, x) in
  g r;
  ()

let () = f 1
