(* TEST
 include stdlib_alpha;
*)

[@@@alert "-unsafe_parallelism"]

open Stdlib_alpha

module Unique_atomic : sig @@ portable
  type 'a t : mutable_data with 'a

  val make : 'a @ unique -> 'a t
  val exchange : ('a : value mod portable contended).
    'a t @ contended -> 'a @ unique -> 'a @ unique
end = struct
  type 'a t = 'a Atomic.t

  external make : 'a @ unique -> 'a t @@ portable = "%makemutable"
  external exchange : ('a : value mod portable contended).
    'a t @ contended -> 'a @ unique -> 'a @ unique
    @@ portable = "%atomic_exchange"
end

module Ops = struct
  type 'a t = Yield : int -> unit t
end
module Eff = Effect.Make(Ops)

type state : value mod contended portable =
  | Used
  | Cont : { key : 'k Capsule.Key.t;
             cont : ((unit, (unit, unit) Eff.Result.t, unit)
               Effect.Continuation.t, 'k) Capsule.Data.t } -> state
  [@@unsafe_allow_any_mode_crossing
               "CR layouts v2.8: GADT mode crossing"]

type t = state Unique_atomic.t

(* CR dkalinichenko: create [unique] versions of functions in [Capsule]. *)

module Unique_capsule = struct
  let create (f : (unit -> 'a @ unique)) =
    Obj.magic_unique (Capsule.Data.create f)

  let wrap ax (x @ unique) =
    Obj.magic_unique (Capsule.Data.wrap ax x)

  let unwrap ax (t @ unique) =
    Obj.magic_unique (Capsule.Data.unwrap ax t)

  let access k (f @ once portable) =
    Capsule.Key.access k (Obj.magic_portable (Obj.magic_many f))
end

let create f =
  let P key = Capsule.create () in
  let cont = Unique_capsule.create (fun () -> Eff.fiber (fun h () -> f h)) in
  Unique_atomic.make (Cont { key; cont })

exception Already_used

let next t =
  match Unique_atomic.exchange t Used with
  | Used -> raise Already_used
  | Cont {key; cont} ->
      let res, key =
        Unique_capsule.access key (fun (ax : _ Capsule.Access.t) ->
          let cont = Unique_capsule.unwrap ax cont in
          match Effect.continue cont () [] with
          | Eff.Value () -> None
          | Eff.Exception e -> raise e
          | Eff.Operation(Yield i, (cont : (unit, _, _) Effect.Continuation.t)) ->
              Some (i, Unique_capsule.wrap ax cont))
      in
      (match res with
      | None -> None
      | Some (i, cont) ->
        (match Unique_atomic.exchange t (Cont {key; cont}) with
        | Used -> ()
        | Cont _ -> assert false);
        Some i)
;;

let () =
  print_endline "Starting in domain 0";
  let t = create (fun h ->
    for i = 1 to 10 do
      print_string "Jump! ";
      Eff.perform h (Yield i)
    done)
  in
  let rec loop () =
    Domain.join (Domain.Safe.spawn (fun () ->
      match next t with
      | None -> ()
      | Some i ->
        Printf.printf "Now in domain %d\n" i;
        loop ()));
  in loop ()
;;
