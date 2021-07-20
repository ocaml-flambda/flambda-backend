let x = (fun () -> (fun _ -> Lazy.force (lazy "")) ()) ()


let rec foo = lazy (fun x : int -> x)
and t = lazy (Lazy.force foo)
