(* TEST
 include stdlib_alpha;
*)

module Effect = Stdlib_alpha.Effect

open Effect

type 'a op1 = Xchg : int -> int op1

module Eff1 = Effect.Make (struct
    type 'a t = 'a op1
  end)

let comp1 h =
  let a = Xchg 0 in
  let x = Eff1.perform h a in
  let b = Xchg 1 in
  let y = Eff1.perform h b in
  x + y
;;

let comp2 h =
  let _ = Eff1.perform h (Xchg 0) in
  raise Not_found
;;

let comp3 h =
  let _ = Eff1.perform h (Xchg 0) in
  int_of_string "fdjsl"
;;

let handle comp =
  (* try *)
  let rec handle = function
    | Eff1.Value x -> x - 30
    | Eff1.Exception _ -> 42
    | Eff1.Operation(Xchg n, k) -> handle (continue k (n + 17) [])
  in
  Format.printf "%d@." (handle (Eff1.run comp))
  (*with Not_found -> assert false*)
;;


let () =
  handle comp1;
  handle comp2;
  handle comp3
;;

type 'a status =
  | Complete of 'a
  | Suspended of
      { msg : int
      ; cont : (int, 'a, unit) Eff1.Continuation.t
      }

let handle = function
  | Eff1.Value v -> Complete v
  | Eff1.Exception e -> raise e
  | Eff1.Operation(Xchg msg, cont) -> Suspended { msg;  cont }

let step (f : (*local_*) _ -> 'a) () : 'a status =
  handle (Eff1.run f)
;;

let rec run_both a b =
  match a (), b () with
  | Complete va, Complete vb -> va, vb
  | Suspended { msg = m1; cont = k1 }, Suspended { msg = m2; cont = k2 } ->
    run_both
      (fun () -> handle (continue k1 m2 []))
      (fun () -> handle (continue k2 m1 []))
  | _ -> failwith "Improper synchronization"
;;

let comp2 h = Eff1.perform h (Xchg 21) * Eff1.perform h (Xchg 21)

let () =
  let x, y = run_both (step comp1) (step comp2) in
  Format.printf ">> %d %d@." x y
;;

type ('a, 't) op2 =
  | Fork : ((*local_*) 't Handler.t -> unit) -> (unit, 't) op2
  | Yield : (unit, 't) op2
  | Xchg : int -> (int, 't) op2

module Eff2 = Effect.Make_rec (struct
    type ('a, 't) t = ('a, 't) op2
  end)

let fork h f = Eff2.perform h (Fork f)
let yield h () = Eff2.perform h Yield
let xchg h v = Eff2.perform h (Xchg v)

(* A concurrent round-robin scheduler *)
let run main : unit =
  let exchanger : (int * (int, unit, unit) Eff2.Continuation.t) option ref =
    ref None
  in
  (* waiting exchanger *)
  let run_q = Queue.create () in
  (* scheduler queue *)
  let dequeue () =
    if Queue.is_empty run_q
    then () (* done *)
    else (
      let task = Queue.pop run_q in
      task ())
  in
  let rec enqueue : type a. (a, _, _) Eff2.Continuation.t -> a -> unit =
    fun k v ->
      let task () = handle (continue k v []) in
      Queue.push task run_q
  and handle = function
    | Eff2.Value () -> dequeue ()
    | Eff2.Exception e ->
        print_endline (Printexc.to_string e);
        dequeue ()
    | Eff2.Operation(Yield, k) ->
        enqueue k ();
        dequeue ()
    | Eff2.Operation(Fork f, k) ->
        enqueue k ();
        handle (Eff2.run f)
    | Eff2.Operation(Xchg n, k) -> begin
        match !exchanger with
        | Some (n', k') ->
            exchanger := None;
            enqueue k' n;
            handle (continue k n' [])
        | None ->
            exchanger := Some (n, k);
            dequeue ()
      end
  in
  handle (Eff2.run main)
;;

let _ =
  run (fun h ->
    fork h (fun h ->
      Format.printf "[t1] Sending 0@.";
      let v = xchg h 0 in
      Format.printf "[t1] received %d@." v);
    fork h (fun h ->
      Format.printf "[t2] Sending 1@.";
      let v = xchg h 1 in
      Format.printf "[t2] received %d@." v))
;;

(*****)

type 'a op3 =
  | E : string op3

type 'a op4 =
  | F : string op4

module Eff3 = Effect.Make (struct
    type 'a t = 'a op3
  end)

module Eff4 = Effect.Make (struct
    type 'a t = 'a op4
  end)

let foo h1 h2 = Eff4.perform h2 F ^ " " ^ Eff3.perform h1 E ^ " " ^ Eff4.perform h2 F

let bar h =
  let rec handle = function
    | Eff3.Value x -> x
    | Eff3.Exception e -> raise e
    | Eff3.Operation(E, k) -> handle (continue k "Coucou!" [h])
  in
  handle (Eff3.run_with [h] (fun [h1; h2] -> foo h1 h2)) [@nontail]
;;

let baz () =
  let rec handle = function
    | Eff4.Value x -> x
    | Eff4.Exception e -> raise e
    | Eff4.Operation(F, k) ->
      handle (continue k "Hello, world!" [])
  in
  handle (Eff4.run bar)
;;

let () = Format.printf "%s@." (baz ())

(****)

let () =
  Format.printf
    "%s@."
    (let rec handle = function
        | Eff4.Value x -> x
        | Eff4.Exception e -> raise e
        | Eff4.Operation(F, k) -> handle (discontinue k Not_found [])
     in
     handle (Eff4.run
               (fun h ->
                  try Eff4.perform h F with
                  | Not_found -> "Discontinued")))
;;

let () =
  try
    Format.printf "%d@."
    @@ let rec handle = function
        | Eff1.Value x -> x
        | Eff1.Exception e -> raise e
        | Eff1.Operation(Xchg _, k) ->
          handle (continue k 21 []) + handle (continue k 21 [])
       in
       handle (Eff1.run (fun h -> Eff1.perform h (Xchg 0)))
  with
  | Continuation_already_resumed -> Format.printf "One-shot@."
;;

(****)

type ('a, 'p) op5 =
  | Yield : 'p -> (unit, 'p) op5

module Eff5 = Effect.Make1 (struct
    type ('a, 'p) t = ('a, 'p) op5
  end)

let invert (type a) ~(iter : (*local_*) (a -> unit) -> unit) : a Seq.t =
  fun () ->
    let rec handle = function
      | Eff5.Value () -> Seq.Nil
      | Eff5.Exception e -> raise e
      | Eff5.Operation(Yield v, k) ->
          Seq.Cons (v, fun () -> handle (continue k () []))
    in
    handle (Eff5.run (fun h -> iter (fun x -> Eff5.perform h (Yield x))[@nontail]))
;;

let string_iter f s =
  for i = 0 to String.length s - 1 do f (String.unsafe_get s i) done

let s = invert ~iter:(fun yield -> string_iter yield "OCaml")
let next = Seq.to_dispenser s

let rec loop () =
  match next () with
  | Some c ->
    Format.printf "%c" c;
    loop ()
  | None -> Format.printf "@."
;;

let () = loop ()

(****)

type 'a op6 =
  | Send : int -> unit op6
  | Recv : int op6

module Eff6 = Effect.Make (struct
    type 'a t = 'a op6
  end)

let run comp =
  let rec handle_send = function
    | Eff6.Value x -> x
    | Eff6.Exception e -> raise e
    | Eff6.Operation(Send n, k) -> handle_recv n (continue k () [])
    | Eff6.Operation(Recv, _) -> failwith "protocol violation"
  and handle_recv n = function
    | Eff6.Value x -> x
    | Eff6.Exception e -> raise e
    | Eff6.Operation(Recv, k) -> handle_send (continue k n [])
    | Eff6.Operation(Send _, _) -> failwith "protocol violation"
  in
  handle_send (Eff6.run comp)
;;

let () =
  run (fun h ->
    Format.printf "Send 42@.";
    Eff6.perform h (Send 42);
    Format.printf "Recv: %d@." (Eff6.perform h Recv);
    Format.printf "Send 43@.";
    Eff6.perform h (Send 43);
    Format.printf "Recv: %d@." (Eff6.perform h Recv))
;;
