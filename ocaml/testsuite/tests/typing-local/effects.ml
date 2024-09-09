open Effect
open Effect.Deep

type _ Effect.t += Xchg: int -> int t

external opaque : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

let[@inline never] local_alloc x y =
  Gc.compact ();
  let in_minor = opaque (opaque x, opaque y) in
  Gc.full_major ();
  exclave_ (opaque (opaque in_minor, opaque in_minor))

let comp1 () =
  let p = local_alloc 1 2 in
  perform (Xchg (fst (fst (opaque p))))
    + (opaque (
        let q = opaque (local_ (opaque 100, opaque 200)) in
        fst q
      ))
    + perform (Xchg (snd (snd (opaque p))))

let[@inline never] f () =
    try_with comp1 ()
    { effc = fun (type a) (eff: a t) ->
        Gc.compact ();
        match eff with
        | Xchg n -> Some (fun (k: (a, _) continuation) ->
            Gc.compact ();
            let q = opaque (local_ (opaque 10, opaque 20)) in
            continue k (n + fst q))
        | _ -> None
    }

let () = Printf.printf "%d\n%!" (f ())
