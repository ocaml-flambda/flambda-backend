[@@@ocaml.warning "+a-30-40-41-42"]

let max_shrink_steps = 1000

type t =
  { verbose : bool;
    seed : int;
    mutable something_has_failed : bool
  }

let create ?(verbose = false) ?(seed = 0) () =
  { verbose; seed; something_has_failed = false }

module Failure = struct
  type t =
    | Returned_false
    | Raised of exn
end

module Outcome = struct
  type 'repr t =
    | Success
    | Failure of
        { failure : Failure.t;
          counterexample : 'repr;
          shrink_steps : int;
          attempt : int
        }

  let is_success = function Success -> true | Failure _ -> false

  let report t ~type_ ~attempts ~duration =
    match t with
    | Success -> Format.eprintf "PASSED %d attempts (%f s)@." attempts duration
    | Failure { failure; counterexample; shrink_steps; attempt } ->
      let explanation =
        match failure with
        | Returned_false -> "returned false"
        | Raised (Assert_failure _) -> "failed assertion"
        | Raised _ -> "raised exception"
      in
      Format.eprintf "FAILED (%s) after %d/%d attempts@." explanation attempt
        attempts;
      begin
        match failure with
        | Returned_false -> ()
        | Raised exn -> Format.eprintf "%s@." (Printexc.to_string exn)
      end;
      Format.eprintf "@[<hov 2>Counterexample (shrunk %d times):@ %a@]@."
        shrink_steps (Type.print type_) counterexample
end

module Outcome_of_one_attempt = struct
  type t =
    | Success
    | Failure of Failure.t
end

let check_once _t ~f ~(type_ : (_, 'repr) Type.t) (repr : 'repr) :
    Outcome_of_one_attempt.t =
  match f (Type.value type_ repr) with
  | true -> Success
  | false -> Failure Returned_false
  | exception e -> Failure (Raised e)

let check0 ?(n = 1000) ?seed t ~type_ ~f ~name =
  Format.eprintf "%s: " name;
  let start = Sys.time () in
  let seed = seed |> Option.value ~default:t.seed in
  let r = Splittable_random.of_int seed in
  let i = ref 1 in
  let outcome = ref Outcome.Success in
  while Outcome.is_success !outcome && !i <= n do
    let repr = Type.generate_repr type_ r in
    if t.verbose
    then
      Format.eprintf "@[<hov 2>Attempt %d/%d:@ %a@]@." !i n (Type.print type_)
        repr;
    match check_once t ~f ~type_ repr with
    | Success -> incr i
    | Failure failure ->
      let rec retry ~shrink_steps ~(seq : _ Seq.t) ~last_counterexample
          ~last_failure ~last_shrink_steps : _ Outcome.t =
        let done_ () : _ Outcome.t =
          Failure
            { failure = last_failure;
              counterexample = last_counterexample;
              shrink_steps = last_shrink_steps;
              attempt = !i
            }
        in
        if shrink_steps >= max_shrink_steps
        then done_ ()
        else
          match seq () with
          | Nil -> done_ ()
          | Cons (repr, seq) -> begin
            match check_once t ~f ~type_ repr with
            | Success ->
              retry ~shrink_steps:(shrink_steps + 1) ~last_counterexample
                ~last_failure ~last_shrink_steps ~seq
            | Failure failure ->
              let seq = Type.shrink type_ repr in
              let last_counterexample = repr in
              let last_failure = failure in
              let last_shrink_steps = shrink_steps in
              retry ~shrink_steps:(shrink_steps + 1) ~last_counterexample
                ~last_shrink_steps ~last_failure ~seq
          end
      in
      let shrink_steps = 0 in
      let seq = Type.shrink type_ repr in
      let last_counterexample = repr in
      let last_failure = failure in
      let last_shrink_steps = 0 in
      outcome
        := retry ~seq ~shrink_steps ~last_counterexample ~last_failure
             ~last_shrink_steps
  done;
  let finish = Sys.time () in
  let duration = finish -. start in
  Outcome.report !outcome ~type_ ~attempts:n ~duration;
  if not (Outcome.is_success !outcome) then t.something_has_failed <- true

let check :
    type a reprs.
    ?n:int ->
    ?seed:int ->
    t ->
    types:(a, reprs, bool) Tuple.Of2(Type.T).t ->
    f:a ->
    name:string ->
    unit =
 fun ?n ?seed t ~types ~f ~name ->
  let run type_ f = check0 ?n ?seed t ~type_ ~f ~name in

  match types with
  | [] -> run Type.unit (fun () -> f)
  | [ty] -> run ty f
  | [ty1; ty2] -> run (Type.pair ty1 ty2) (fun (a, b) -> f a b)
  | [ty1; ty2; ty3] -> run (Type.triple ty1 ty2 ty3) (fun (a, b, c) -> f a b c)
  | [ty1; ty2; ty3; ty4] ->
    run (Type.quad ty1 ty2 ty3 ty4) (fun (a, b, c, d) -> f a b c d)
  | ty1 :: ty2 :: ty3 :: types ->
    (* Could do more to pack the remaining arguments together but I've
       over-engineered this enough as is *)
    run
      (Type.quad ty1 ty2 ty3 (Type.tuple types))
      (fun (a, b, c, tup) -> Tuple.call ~f:(f a b c) tup)

let something_has_failed t = t.something_has_failed
