let default_attempts = 1000

let default_seed = 0

let default_verbose = false

(* Guard against non-terminating shrinkers. This should be quite large, as a
   well-reduced test case is worth waiting for. *)
let max_shrink_steps = 100000

module Failure = struct
  type t =
    | Returned_false
    | Raised of exn
end

module Outcome = struct
  type t =
    | Success
    | Failure :
        { failure : Failure.t;
          counterexample : 'repr;
          arbitrary_impl : ('a, 'repr) Arbitrary.t;
          shrink_steps : int;
          attempt : int
        }
        -> t

  let is_success = function Success -> true | Failure _ -> false

  let report t ~attempts ~duration =
    match t with
    | Success -> Format.eprintf "PASSED %d attempts (%f s)@." attempts duration
    | Failure { failure; counterexample; arbitrary_impl; shrink_steps; attempt }
      ->
      let explanation =
        match failure with
        | Returned_false -> "returned false"
        | Raised (Assert_failure _) -> "failed assertion"
        | Raised _ -> "raised exception"
      in
      Format.eprintf "FAILED (%s) after %d/%d attempts@." explanation attempt
        attempts;
      (match failure with
      | Returned_false -> ()
      | Raised exn -> Format.eprintf "%s@." (Printexc.to_string exn));
      Format.eprintf "@[<hov 2>Counterexample (shrunk %d times):@ %a@]@."
        shrink_steps
        (Arbitrary.print arbitrary_impl)
        counterexample
end

type t = { mutable outcomes_rev : Outcome.t list }

let create () = { outcomes_rev = [] }

module Outcome_of_one_attempt = struct
  type t =
    | Success
    | Failure of Failure.t
end

let check_once _t ~f ~(arbitrary_impl : (_, 'repr) Arbitrary.t) (repr : 'repr) :
    Outcome_of_one_attempt.t =
  match f (Arbitrary.value arbitrary_impl repr) with
  | true -> Success
  | false -> Failure Returned_false
  | exception e -> Failure (Raised e)

let check0 ?(n = default_attempts) ?(seed = default_seed)
    ?(verbose = default_verbose) t ~arbitrary_impl ~f ~name =
  Format.eprintf "%s: " name;
  let start = Sys.time () in
  let r = Splittable_random.of_int seed in
  let i = ref 1 in
  let outcome = ref Outcome.Success in
  while Outcome.is_success !outcome && !i <= n do
    let repr = Arbitrary.generate_repr arbitrary_impl r in
    if verbose
    then
      Format.eprintf "@[<hov 2>Attempt %d/%d:@ %a@]@." !i n
        (Arbitrary.print arbitrary_impl)
        repr;
    match check_once t ~f ~arbitrary_impl repr with
    | Success -> incr i
    | Failure failure ->
      let rec retry ~shrink_steps ~(seq : _ Seq.t) ~last_counterexample
          ~last_failure ~last_shrink_steps : Outcome.t =
        let done_ () : Outcome.t =
          Failure
            { arbitrary_impl;
              failure = last_failure;
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
          | Cons (repr, seq) -> (
            match check_once t ~f ~arbitrary_impl repr with
            | Success ->
              retry ~shrink_steps:(shrink_steps + 1) ~last_counterexample
                ~last_failure ~last_shrink_steps ~seq
            | Failure failure ->
              let seq = Arbitrary.shrink arbitrary_impl repr in
              let last_counterexample = repr in
              let last_failure = failure in
              let last_shrink_steps = shrink_steps in
              retry ~shrink_steps:(shrink_steps + 1) ~last_counterexample
                ~last_shrink_steps ~last_failure ~seq)
      in
      let shrink_steps = 0 in
      let seq = Arbitrary.shrink arbitrary_impl repr in
      let last_counterexample = repr in
      let last_failure = failure in
      let last_shrink_steps = 0 in
      outcome
        := retry ~seq ~shrink_steps ~last_counterexample ~last_failure
             ~last_shrink_steps
  done;
  let finish = Sys.time () in
  let duration = finish -. start in
  Outcome.report !outcome ~attempts:n ~duration;
  t.outcomes_rev <- !outcome :: t.outcomes_rev

let check :
    type a reprs.
    ?n:int ->
    ?seed:int ->
    ?verbose:bool ->
    t ->
    arbitrary_impls:(a, reprs, bool) Tuple.Of2(Arbitrary.T).t ->
    f:a ->
    name:string ->
    unit =
 fun ?n ?seed ?verbose t ~arbitrary_impls ~f ~name ->
  let run arbitrary_impl f =
    check0 ?n ?seed ?verbose t ~arbitrary_impl ~f ~name
  in
  match arbitrary_impls with
  | [] -> run Arbitrary.unit (fun () -> f)
  | [arb] -> run arb f
  | [arb1; arb2] -> run (Arbitrary.pair arb1 arb2) (fun (a, b) -> f a b)
  | [arb1; arb2; arb3] ->
    run (Arbitrary.triple arb1 arb2 arb3) (fun (a, b, c) -> f a b c)
  | [arb1; arb2; arb3; arb4] ->
    run (Arbitrary.quad arb1 arb2 arb3 arb4) (fun (a, b, c, d) -> f a b c d)
  | arb1 :: arb2 :: arb3 :: arbitrary_impls ->
    (* Could do more to pack the remaining arguments together but I've
       over-engineered this enough as is *)
    run
      (Arbitrary.quad arb1 arb2 arb3 (Arbitrary.tuple arbitrary_impls))
      (fun (a, b, c, tup) -> Tuple.call ~f:(f a b c) tup)

(* CR-someday lmaurer: Could also return something richer, although the full
   [Outcome.t] may be a bit much. *)

let failure_count t =
  List.length
    (List.filter
       (fun outcome -> not (Outcome.is_success outcome))
       t.outcomes_rev)
