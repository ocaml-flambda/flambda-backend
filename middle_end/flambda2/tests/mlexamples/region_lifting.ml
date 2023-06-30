open struct
  (* Hide the creation of the ref to prevent unboxing *)
  let[@inline never] make_local_thing x = local_ ref x

  let[@inline always] use_local_thing x =
    (* Calling [make_local_thing] forces us to have a region on hand *)
    let r = make_local_thing x in
    let v = !r in
    (* good case: *)
    v, ()
  (* also good case: *)
  (* [%exclave] (v, ()) *)
  (* bad case: *)
  (* if v > 0 then v, 1 else [%exclave] (v, 2) *)
end

let f x =
  let a, () = use_local_thing x in
  a + a

(* Make ocamlformat leave our lovely code blocks alone *)
let [@ocamlformat "disable"] () =
  (* Without local allocations, after desugaring and inlining we have
     {v
       let f x =
         let a =
           let r = make_local_thing x in
           let v = !r in
           v, ()
         in
         let x = a.0 in
         x + x
     v}

     which lifting transforms into
     {v
       let r = make_local_thing x in
       let v = !r in
       let a = v, () in
       let x = a.0 in
       x + x
     v}

     which easily simplifies to
     {v
       let r = make_local_thing x in
       let v = !r in
       let x = v in
       x + x
     v}

     With local allocations, however, we get
     {v
       let f x =
         let a =
           region (
             let r = make_local_thing x in
             let v = !r in
             v, ())
         in
         let x = a.0 in
         x + x
     v}

     At this point, flambda1 is stuck: since [v] is not visible at the
     definition of [x], we can't substitute away [a.0], so we can't eliminate
     the allocation of the pair.

     Region lifting gets us here:
     {v
       let f x =
         region (
           let r = make_local_thing x in
           let v = !r in
           let a = v, () in
           exclave (
             let x = a.0 in
             x + x))
     v}

     From there, it's again smooth sailing. Note that we had to introduce an
     exclave in order to leave the region yet keep the same variables in scope.
     The one thing that gets moved out of the region is the result [v, ()],
     which was going to escape the region anyway and thus cannot be unsafe to
     move out of it.

     Things get hairier if there are already exclaves present. For instance, if
     we manually put the original [v, ()] in an exclave using [%exclave], we
     get (after desugaring and inlining):
     {v
       let f x =
         let a =
           region (
             let r = make_local_thing x in
             let v = !r in
             exclave (v, ()))
         in
         let x = a.0 in
         x + x
     v}

     This is no real problem so long as we're careful to use this existing
     exclave when we lift the region: in fact, lifting produces nearly the same
     code (the binding of [a] will happen in the exclave instead of the region).
     An exclave in an if branch is a bigger problem, though:
     {v
       let f x =
         let a =
           region (
             let r = make_local_thing x in
             let v = !r in
             if ... then
               v, 1
             else
               exclave (v, 2))
         in
         let x = a.0 in
         x + x
     v}

     Now there's nothing we can do without join points or full continuations
     (unless we're willing to duplicate all the code after the region or
     put it in a function, which we're not). *)
   ()
