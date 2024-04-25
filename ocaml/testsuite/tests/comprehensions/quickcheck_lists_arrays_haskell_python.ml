(* -*- compile-command: "ocamlopt -w +A-4-40-42-44 str.cmxa unix.cmxa quickcheck_lists_arrays_haskell_python.ml -o quickcheck-lists-arrays-haskell-python && ./quickcheck-lists-arrays-haskell-python"; -*- *)

(* NB: This file was great for validating comprehensions, but we can't put it in
   the compiler test suite: it spins up three different REPL processes, one each
   for Haskell, Python, and OCaml, and we can't count on the first two existing.
   But it would be a shame to delete this code and just leave
   `comprehensions_from_quickcheck.ml`; if things change, it's good to have
   access to QuickCheck.  Consequently, the code is still here, but as it's not
   built it is actively bitrotting. *)

module No_polymorphic_compare = struct
  let ( = )      = Int.equal
  let ( < )  x y = Int.compare x y <  0
  let ( > )  x y = Int.compare x y >  0
  let ( <= ) x y = Int.compare x y <= 0
  let ( >= ) x y = Int.compare x y >= 0
end

open No_polymorphic_compare

module Util = struct
  module List_monad = struct
    let pure x = [x]
    let bind xs f = List.concat_map f xs

    let (let*)      = bind
    let (let+) xs f = List.map f xs

    (* I think this is right *)
    let (and*) xs ys =
      let* x = xs in
      let+ y = ys in
      x,y
    let (and+) = (and*)

    let rec traverse f = function
      | [] ->
          pure []
      | x :: xs ->
          let+ y  = f x
          and+ ys = traverse f xs in
          y :: ys
  end

  let rec take_while p = function
    | x :: xs when p x -> x :: take_while p xs
    | _ -> []

  let guard c x = if c then [x] else []

  let max x y = if x > y then x else y

  let range_to start stop =
    List.init (max 0 (stop - start + 1)) (fun i -> start + i)

  let range_downto start stop =
    List.init (max 0 (start - stop + 1)) (fun i -> start - i)

  (* For repeatability *)
  external random_seed : unit -> int array = "caml_sys_random_seed"

  let output_line oc str = begin
    output_string oc str;
    output_char oc '\n';
    flush oc
  end
end

module QuickCheck = struct
  type 'a prop_result =
    | OK
    | Failed_with of 'a

  type 'a failure_data =
    | Data      of 'a
    | Exception of exn

  type ('a, 'b) failure =
    { counterexample : 'a
    ; data           : 'b failure_data
    ; tests          : int
    ; shrinks        : int }

  type ('a, 'b) result =
    | Passed
    | Failed  of ('a, 'b) failure

  module Print (Printf : sig
      type destination
      type result
      val printf : destination -> ('a, destination, result) format -> 'a
    end) =
  struct
    (* This only works with some words but that's ok *)
    let quantity dst (n, thing) =
      Printf.printf dst "%d %s%s" n thing (if n = 1 then "" else "s")

    let tests dst tests = quantity dst (tests,   "test")

    let and_shrinks dst = function
      | 0       -> Printf.printf dst ""
      | shrinks -> Printf.printf dst " and %a" quantity (shrinks, "shrink")

    let and_shrinks_and_iteration dst = function
      | shrinks, 0 ->
          and_shrinks dst shrinks
      | shrinks, iteration ->
          Printf.printf dst " and %d.%d shrinks" shrinks iteration
  end

  module SPrint = Print (struct
      type destination = unit
      type result      = string
      let printf ()    = Printf.sprintf
    end)

  module FPrint = Print (struct
      type destination = out_channel
      type result      = unit
      let printf       = Printf.fprintf
    end)

  module Reporter = struct
    type t =
      { report_test      : int -> unit
      ; report_shrink    : tests:int -> shrinks:int -> iteration:int -> unit
      ; finish_reporting : unit -> unit
      }

    let silent =
      { report_test      = Fun.const ()
      ; report_shrink    = (fun ~tests:_ ~shrinks:_ ~iteration:_ -> ())
      ; finish_reporting = Fun.const ()
      }

    type interactive_output_mode =
      | Backspace_moves
      | Backspace_deletes

    let interactive_output_mode oc =
      match Unix.(isatty (descr_of_out_channel oc)) with
      | true ->
          Some (if Option.is_some (Sys.getenv_opt "INSIDE_EMACS") ||
                   Option.is_some (Sys.getenv_opt "EMACS")
                then Backspace_deletes
                else Backspace_moves)
      | false | exception _ ->
          None

    let interactive_main iom oc =
      (* This line-clearing technique was taken from Haskell's QuickCheck,
         although sadly it doesn't work in Emacs *)
      let string_as_char s c = String.make (String.length s) c in
      let backspace_prev_line = ref "" in
      let clear_prev_line = match iom with
        | Backspace_moves -> fun () ->
            output_string oc (string_as_char !backspace_prev_line ' ');
            output_string oc !backspace_prev_line
        | Backspace_deletes -> fun () ->
            output_string oc !backspace_prev_line
      in
      let move_cursor_for_this_line = match iom with
        | Backspace_moves   -> output_string oc
        | Backspace_deletes -> Fun.const ()
      in
      let report fstr =
        Printf.ksprintf
          (fun line ->
             clear_prev_line ();
             let backspace_this_line = string_as_char line '\b' in
             output_string oc line;
             move_cursor_for_this_line backspace_this_line;
             flush oc;
             backspace_prev_line := backspace_this_line)
          fstr
      in
      { report_test   = (fun tests ->
          report "(%a...)" SPrint.tests tests)
      ; report_shrink = (fun ~tests ~shrinks ~iteration ->
          report "Failed!  (%a%a...)"
            SPrint.tests                     tests
            SPrint.and_shrinks_and_iteration (shrinks, iteration))
      ; finish_reporting = (fun () ->
          clear_prev_line ();
          flush oc)
      }

    let main oc = match interactive_output_mode oc with
      | Some iom -> interactive_main iom oc
      | None     -> silent
  end

  let rec find_counterexample ~report iteration prop = function
    | [] -> None
    | x :: xs ->
        report ~iteration;
        match prop x with
        | OK               -> find_counterexample ~report (iteration+1) prop xs
        | Failed_with data -> Some (x, Data data)
        | exception exn    -> Some (x, Exception exn)

  let find_counterexample ?(report = fun ~iteration:_ -> ()) prop =
    find_counterexample ~report 0 prop

  let rec minimize
            ?(report = fun ~shrinks:_ ~iteration:_ -> ()) shrink prop failure =
    match
      find_counterexample ~report:(report ~shrinks:failure.shrinks)
        prop (shrink failure.counterexample)
    with
    | Some (counterexample, data) ->
        minimize ~report shrink prop
          { failure with counterexample; data; shrinks = failure.shrinks + 1 }
    | None ->
        failure

  let test (type a b) ?(reporter = Reporter.silent) n gen shrink prop =
    let exception Counterexample of (a, b) failure in
    let result =
      match
        for tests = 1 to n do
          reporter.report_test tests;
          let x = gen () in
          let stop_with_this_counterexample data =
            raise (Counterexample
                     { counterexample = x; data = data; tests; shrinks = 0 })
          in
          match prop x with
          | OK               -> ()
          | Failed_with data -> stop_with_this_counterexample (Data      data)
          | exception exn    -> stop_with_this_counterexample (Exception exn)
        done
      with
      | () ->
          Passed
      | exception Counterexample failure ->
          Failed (minimize ~report:(reporter.report_shrink ~tests:failure.tests)
                    shrink prop failure)
    in
    reporter.finish_reporting ();
    result

  let main
        ?(seed = Util.random_seed ()) ?(output = stdout)
        max_tests gen shrink print_failure prop =
    let printf fstr = Printf.fprintf output fstr in
    Random.full_init seed;
    match test ~reporter:(Reporter.main output) max_tests gen shrink prop with
    | Passed ->
        printf "OK, passed %a.\n" FPrint.tests max_tests
    | Failed { counterexample; data; tests; shrinks } ->
        let what, odata, print_extra_information = match data with
          | Data data ->
              "Counterexample",
              Some data,
              (fun () -> ())
          | Exception exn ->
              "Exception",
              None,
              (fun () ->
                 printf "  Exception:\n    %s\n"
                   (exn
                    |> Printexc.to_string
                    |> Str.global_replace (Str.regexp "\n") "\n    "))
        in
        printf "Failed with seed [|%s|]!\n"
          (String.concat "; " (Array.to_list (Array.map Int.to_string seed)));
        printf "%s (after %a%a):\n"
          what
          FPrint.tests       tests
          FPrint.and_shrinks shrinks;
        print_failure output counterexample odata;
        print_extra_information ()

  module Generator = struct
    let replicateG n g =
      Array.make n Fun.id |> Array.to_list |> List.map (fun _ -> g ())

    let pick_without_replacement xs =
      let rec go i xs = match i, xs with
        | 0, x :: xs -> x, xs
        | i, x :: xs -> let y, ys = go (i-1) xs
                        in y, x :: ys
        | _, []      -> assert false
      in
      go (Random.int (List.length xs)) xs

    let pick xs = List.nth xs (Random.int (List.length xs))

    let small_int () = Random.int 7 - 3 (* [-3,3] *)
  end

  module Shrink = struct
    let rec del1_and_shrink1 shrink = function
      | [] ->
          [], []
      | x :: xs ->
          let del, shrunk = del1_and_shrink1 shrink xs in
          let cons_x xs' = x :: xs' in
          ( xs                                        :: List.map cons_x del
          , List.map (fun x'  -> x' :: xs) (shrink x) @  List.map cons_x shrunk
          )

    let nonempty_list shrink xs =
      match del1_and_shrink1 shrink xs with
      | [[]], shrunk -> shrunk
      | del,  shrunk -> del @ shrunk

    let list shrink xs =
      let del, shrunk = del1_and_shrink1 shrink xs in
      del @ shrunk

    (* From Haskell's QuickCheck: make it positive, 0, then smaller by jumping
       half the distance each time *)
    let int i =
      let rec halves = function
        | 0 -> []
        | d -> i - d :: halves (d/2)
      in
      Util.guard (i < 0 && i <> Int.min_int) (-i) @
      Util.guard (i <> 0)                    0    @
      halves (i/2)

    (* Allow either one or two shrinks from the given shrinker *)
    let shrink2 shrink x =
      let shrink1 = shrink x in
      shrink1 @ List.concat_map shrink shrink1
  end
end

module Var : sig
  type t = string

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t

  val equal : t -> t -> bool

  val vars : t list
  val wildcard : t
  val pattern_vars : t list
end = struct
  type t = string

  module Set = Set.Make(String)
  module Map = Map.Make(String)

  let equal = String.equal

  let vars =
    List.init 26 (fun i -> String.make 1 (Char.chr (Char.code 'a' + i)))
  let wildcard = "_"
  let pattern_vars = wildcard :: vars
end

module Environment : sig
  type t

  val empty : t
  val of_variables : Var.t list -> t

  val add   : Var.t -> t -> t
  val union : t -> t -> t

  val is_empty : t -> bool
  val is_bound : Var.t -> t -> bool
  val is_free  : Var.t -> t -> bool

  val variables     : t -> Var.t list
  val variables_seq : t -> Var.t Seq.t
end = struct
  include Var.Set

  let of_variables  = of_list
  let is_bound      = mem
  let is_free x env = not (is_bound x env)
  let variables     = elements
  let variables_seq = to_seq
end

module Substitution : sig
  type binding =
    | Deleted
    | Renamed of Var.t

  type t

  val identity   : t
  val delete     : Var.t -> t
  val rename     : Var.t -> Var.t -> t
  val delete_env : Environment.t -> t
  val rename_env : Environment.t -> (Var.t -> Var.t) -> t

  val shadow_env : Environment.t -> t -> t

  val apply : t -> Var.t -> binding option
end = struct
  type binding =
    | Deleted
    | Renamed of Var.t

  include Var.Map

  type nonrec t = binding t

  let identity   = empty
  let delete x   = singleton x Deleted
  let rename x y = singleton x (Renamed y)

  let create_with_env f env =
    of_seq (Seq.map (fun x -> x, f x) (Environment.variables_seq env))

  let delete_env = create_with_env (Fun.const Deleted)

  let rename_env env f = create_with_env (fun x -> Renamed (f x)) env

  let shadow_env env = filter (fun x _ -> Environment.is_free x env)

  let apply subst x = find_opt x subst
end

module Comprehension = struct
  type int_term =
    | Literal  of int
    | Variable of Var.t

  type direction =
    | To
    | Downto

  type iterator =
    | Range    of { start     : int_term
                  ; direction : direction
                  ; stop      : int_term  }
    | Sequence of int_term list

  type binding = { var : Var.t; iterator : iterator }

  type predicate =
    | Positive
    | Negative
    | Nonzero
    | Even
    | Odd

  let all_predicates = [Positive; Negative; Nonzero; Even; Odd]

  type clause =
    | For  of binding list
    | When of predicate * Var.t

  (* We assume the body is a tuple of all the variables in the environment *)
  type t = { env : Environment.t ; clauses : clause list }

  module Bound_vars = struct
    let bindings bs =
      bs |>
      List.filter_map (fun {var; iterator = _} ->
        if Var.equal var Var.wildcard
        then None
        else Some var) |>
      Environment.of_variables

    let clauses =
      List.fold_left
        (fun env -> function
           | For bs -> Environment.union (bindings bs) env
           | When _ -> env)
        Environment.empty
  end

  module Generator = struct
    open QuickCheck.Generator

    let in_scope_var env = pick (Environment.variables env)

    let int_term env =
      if not (Environment.is_empty env) && Random.int 10 < 1 then
        Variable (in_scope_var env)
      else
        Literal (small_int ())

    let iterator env =
      if Random.bool ()
      then Range { start     = int_term env
                 ; direction = if Random.bool () then To else Downto
                 ; stop      = int_term env }
      else Sequence (replicateG (Random.int 8) (fun () -> int_term env))
      (* Both Ranges and Sequences can range from length 0 to 7 (inclusive),
         although with different probabilities *)

    let predicate () =
      match Random.int 5 with
      | 0 -> Positive
      | 1 -> Negative
      | 2 -> Nonzero
      | 3 -> Even
      | 4 -> Odd
      | _ -> assert false

    (* Generates bindings that don't share variables *)
    let bindings env sz =
      let rec go ~bindings ~available ~used = function
        | 0 ->
            (* We reverse the list because [_] becomes slightly more likely for
               later-generated values, and this shifts them towards the end of
               the for-and clause *)
            List.rev bindings, used
        | n ->
            let var, available = pick_without_replacement available in
            let available, used =
              if Var.equal var Var.wildcard
              then Var.wildcard :: available, used
              else available, Environment.add var used
            in
            let bindings = { var; iterator = iterator env } :: bindings in
            go ~bindings ~available ~used (n-1)
      in
      go
        ~bindings:[]
        ~available:Var.pattern_vars
        ~used:Environment.empty
        (Random.int sz + 1)

    let clause env sz =
      if not (Environment.is_empty env) && Random.int 4 < 1 then
        When(predicate (), in_scope_var env), env
      else
        let bs, env' = bindings env sz in
        For bs, Environment.union env env'

    let comprehension () =
      let clause_n = Random.int 5 + 1 (* [1,5] *) in
      let for_max  = (7 - clause_n) (* [2,6] *) in
      let rec go env i =
        if i = clause_n then
          [], env
        else
          let b,  env'  = clause env for_max in
          let bs, env'' = go (Environment.union env env') (i+1) in
          b :: bs, env''
      in
      let clauses, env = go Environment.empty 0 in
      {env; clauses}
  end

  module Shrink = struct
    open QuickCheck.Shrink

    (* [-3,3], in increasing order of "complexity" *)
     let all_small_ints =
      let pos = List.init 3 (( + ) 1) in
      let neg = List.map Int.neg pos in
      0 :: (pos @ neg)

    let all_small_int_lits = List.map (fun n -> Literal n) all_small_ints

    let pattern_var x = Util.take_while (fun p -> x <> p) Var.pattern_vars

    let int_term = function
      | Literal  n -> List.map (fun n -> Literal n) (int n)
      | Variable _ -> all_small_int_lits

    let iterator = function
      | Range { start; direction; stop } ->
          [Sequence [start]; Sequence [stop]] @
          Util.guard
            (match direction with Downto -> true | To -> false)
            (Range { start = stop; direction = To; stop = start }) @
          List.map
            (fun start -> Range { start; direction; stop })
            (int_term start) @
          List.map
            (fun stop  -> Range { start; direction; stop })
            (int_term stop) @
          (match start, stop with
           | Literal start, Literal stop ->
               let range = match direction with
                 | To     -> Util.range_to
                 | Downto -> Util.range_downto
               in
               [Sequence (List.map (fun n -> Literal n) (range start stop))]
           | Variable _, _ | _, Variable _ -> [])
      | Sequence seq ->
          List.map (fun seq -> Sequence seq) (list int_term seq)

    let binding ({var = x; iterator = i} as b) =
      List.map (fun iterator -> {b with iterator}) (iterator    i) @
      List.map (fun var      -> {b with var})      (pattern_var x)

    let predicate p =
      Util.take_while (fun p' -> p <> p') all_predicates

    let parallel_bindings bs =
      (* I think preventing name collisions genuinely requires a separate
         traversal *)
      let env = Bound_vars.bindings bs in
      let rec del1_shrink1 = function
        | [] ->
            [], []
        | ({var = x; iterator = i} as b) :: bs ->
            let del, shrunk = del1_shrink1 bs in
            let cons_b (bs', subst) = b :: bs', subst in
            ( (bs, Substitution.delete x) :: List.map cons_b del
            , List.map
                (fun iterator -> {b with iterator} :: bs, Substitution.identity)
                (iterator i) @
              List.filter_map
                (fun var ->
                   if Environment.is_bound var env
                   then None
                   else Some ({b with var} :: bs,
                              if Var.equal var Var.wildcard
                              then Substitution.delete x
                              else Substitution.rename x var))
                (pattern_var x) @
              List.map cons_b shrunk )
      in
      match del1_shrink1 bs with
      | [[], _], shrunk -> shrunk
      | del,     shrunk -> del @ shrunk

    (* Shrinking-specific substitution: deleted variables become every possible
       value *)
    module Substitute = struct
      open Util.List_monad

      let list elt subst = traverse (elt subst)

      let int_term subst = function
        | Literal  n -> pure (Literal n)
        | Variable x -> match Substitution.apply subst x with
          | None              -> pure (Variable x)
          | Some Deleted      -> all_small_int_lits
          | Some (Renamed x') -> pure (Variable x')

      let iterator subst = function
        | Range { start; direction; stop } ->
            let+ start = int_term subst start
            and+ stop  = int_term subst stop in
            Range { start; direction; stop }
        | Sequence seq ->
            let+ seq = list int_term subst seq in
            Sequence seq

      let rec parallel_bindings subst = function
        | [] ->
            (pure [], Environment.empty)
        | ({var; iterator = i} as b) :: bs ->
            let bss, env = parallel_bindings subst bs in
            ( (let+ iterator = iterator subst i
               and+ bs       = bss in
               {b with iterator} :: bs)
            , Environment.add var env )

      let rec clauses subst = function
        | [] ->
            pure []
        | For bs :: cs ->
            let bss, env = parallel_bindings subst bs in
            let subst    = Substitution.shadow_env env subst in
            let+ cs = clauses subst cs
            and+ bs = bss in
            For bs :: cs
        | (When(pred, x) as c) :: cs ->
            let css = clauses subst cs in
            match Substitution.apply subst x with
            | None ->
                let+ cs = css in
                c :: cs
            | Some Deleted ->
                css
            | Some (Renamed x') ->
                let+ cs = css in
                When(pred, x') :: cs
    end

    let clauses cs =
      let rec del1_shrink1 = function
        | [] ->
            [], []
        | (For bs as c) :: cs ->
            let env = Bound_vars.bindings bs in
            let bss_substs = parallel_bindings bs in
            let del, shrunk = del1_shrink1 cs in
            let cons_c cs' = c :: cs' in
            ( Substitute.clauses (Substitution.delete_env env) cs @
              List.map cons_c del
            , (let open Util.List_monad in
               let* bs, subst = bss_substs in
               let+ cs        = Substitute.clauses subst cs in
               For bs :: cs) @
              List.map cons_c shrunk )
        | (When(pred, x) as c) :: cs ->
            (* By the time we get here, [x] is guaranteed to be in scope;
               otherwise, [Substitute.clauses] would have deleted it *)
            let del, shrunk = del1_shrink1 cs in
            let cons_c cs' = c :: cs' in
            ( cs :: List.map cons_c del
            , List.map (fun pred -> When(pred, x) :: cs) (predicate pred) @
              List.map cons_c shrunk )
      in
      match del1_shrink1 cs with
      | [[]], shrunk -> shrunk
      | del,  shrunk -> del @ shrunk

    let comprehension {env = _; clauses = cs} =
      (* I don't think there's a nice way to either (1) rule out empty lists of
         clauses ahead of time, or (2) compute the environment along the way, so
         we handle both directly via post-processing here. *)
      List.filter_map
        (fun clauses ->
           match clauses with
           | []     -> None
           | _ :: _ -> Some { env = Bound_vars.clauses clauses; clauses })
        (clauses cs)

    (* Shrinking twice simplifies both bugs this found on its first go-round,
       since this way we can shrink both the endpoints of a to/downto range or
       shrink two parallel variable names at once. *)
    let comprehension = QuickCheck.Shrink.shrink2 comprehension
  end

  module To_string = struct
    type ocaml_type =
      | List
      | Mutable_array
      | Immutable_array

    type format =
      | OCaml of ocaml_type
      | Haskell
      | Python

    let surround o c s = o ^ s ^ c

    let parenthesize = surround "(" ")"
    let bracket      = surround "[" "]"
    let spaced       = surround " " " "

    let tokens          = String.concat " "
    let comma_separated = String.concat ", "

    let comprehension_clauses o = match o with
      | OCaml _ | Python -> tokens
      | Haskell          -> comma_separated

    let tuple = function
      | [tok] -> tok
      | toks  -> toks |> comma_separated |> parenthesize

    let sequence = function
      | OCaml List | Haskell | Python -> bracket
      | OCaml Mutable_array           -> surround "[|" "|]"
      | OCaml Immutable_array         -> surround "[:" ":]"

    let mod_ = function
      | OCaml _ -> "mod"
      | Haskell -> "`mod`"
      | Python  -> "%"

    let eq = function
      | OCaml _          -> "="
      | Haskell | Python -> "=="

    let neq = function
      | OCaml _ -> "<>"
      | Haskell -> "/="
      | Python  -> "!="

    let int_term = function
      | Literal  n -> Int.to_string n
      | Variable x -> x

    let succ_int_term = function
      | Literal  n -> Int.to_string (n + 1)
      | Variable x -> x ^ "+1"

    let pred_int_term = function
      | Literal  n -> Int.to_string (n - 1)
      | Variable x -> x ^ "-1"

    let modulo_check o tgt = [mod_ o; "2"; eq o; tgt]

    let predicate o = function
      | Positive -> [], [">";   "0"]
      | Negative -> [], ["<";   "0"]
      | Nonzero  -> [], [neq o; "0"]
      | Even -> begin
          match o with
          | OCaml _ -> ["abs"],  modulo_check o "0"
          | Haskell -> ["even"], []
          | Python  -> [],       modulo_check o "0"
        end
      | Odd -> begin
          match o with
          | OCaml _ -> ["abs"], modulo_check o "1"
          | Haskell -> ["odd"], []
          | Python  -> [],      modulo_check o "1"
        end

    let ocaml_direction = function
      | To     -> "to"
      | Downto -> "downto"

    let binding o {var; iterator} =
      let iter = match iterator with
        | Range {start; direction; stop} -> begin
            match o with
            | OCaml _ ->
                tokens [ "="
                       ; int_term start
                       ; ocaml_direction direction
                       ; int_term stop ]
            | Haskell ->
                let step_sep, format_dotdot = match stop with
                  | Literal n when n < 0 -> " ", spaced
                  | _                    -> "",  Fun.id
                in
                let step = match direction with
                  | To     -> ""
                  | Downto -> "," ^ step_sep ^ pred_int_term start
                in
                tokens [ "<-"
                       ; "[" ^
                           int_term start ^ step ^
                           format_dotdot ".." ^
                           int_term stop ^
                         "]" ]
            | Python ->
                let stop, step = match direction with
                  | To     -> succ_int_term stop, []
                  | Downto -> pred_int_term stop, ["-1"]
                in
                "in range" ^ tuple ([int_term start; stop] @ step)
          end
        | Sequence seq ->
            (* There is one edge case where Haskell can report an ambiguous type
               error: if two variables are drawn from empty lists, and then one
               is enumerated to the other, such as in
               [[(a,b,c) | a <- [], b <- [], c <- [a..b]]], or even more simply
               in [[(a,b) | a <- [], b <- [a..a]]].  Thus, if we have an empty
               list in Haskell, we give it a type. *)
            let maybe_type_annotation = match o, seq with
              | Haskell, [] -> ["::"; "[Int]"]
              | _, _ -> []
            in
            let sep = match o with
              | OCaml _          -> ";"
              | Haskell | Python -> ","
            in
            let seq = seq
                      |> List.map int_term
                      |> String.concat (sep ^ " ")
                      |> sequence o
            in
            let bind = match o with
              | OCaml _ | Python -> "in"
              | Haskell          -> "<-"
            in
            tokens ([bind; seq] @ maybe_type_annotation)
      in
      tokens [var; iter]

    (* In Haskell and Python, parallel bindings are interpreted as sequential
       bindings.  Python has other problems, so we need a heavier hammer (see
       [Make_all_variables_unique]), but for Haskell, this is the only
       difference we need to address.  It doesn't cause problems unless (1) a
       variable [x] is in scope for the parallel bindings, (2) one of the
       parallel bindings binds [x] to something new, and (3) [x] is used on the
       right-hand side of a later binding.  In this case, Haskell will see the
       new binding of [x], which will shadow the old one; in OCaml, as these are
       all in parallel, this is not the case.  This function renames all such
       variables to [outer_x], with the given let-binding construct. *)
    let protect_parallel_bindings let_clause bindings =
      let (_bound_vars, _free_vars, outer_lets), bindings =
        List.fold_left_map
          (fun (shadowed, free_vars, outer_lets) {var; iterator} ->
             let protect free_vars = function
               | Variable x when Environment.is_bound x shadowed ->
                   let outer = "outer_" ^ x in
                   let free_vars, outer_let =
                     if Environment.is_bound x free_vars
                     then free_vars,
                          None
                     else Environment.add x free_vars,
                          Some (let_clause outer x)
                   in
                   Variable outer, free_vars, outer_let
               | t ->
                   t, free_vars, None
             in
             let iterator, free_vars, outer_lets' =
               match iterator with
               | Range { start; direction; stop } ->
                   let start, free_vars, start_outer =
                     protect free_vars start
                   in
                   let stop, free_vars, stop_outer =
                     protect free_vars stop
                   in
                   let outer_lets' =
                     List.filter_map Fun.id [start_outer; stop_outer]
                   in
                   Range { start; direction; stop }, free_vars, outer_lets'
               | Sequence seq ->
                   let rev_seq, free_vars, outer_lets' =
                     List.fold_left
                       (fun (rev_ts, free_vars, outer_lets') t ->
                          let t, free_vars, outer = protect free_vars t in
                          t :: rev_ts,
                          free_vars,
                          Option.fold
                            ~none:Fun.id ~some:List.cons outer outer_lets')
                       ([], free_vars, [])
                       seq
                   in
                   Sequence (List.rev rev_seq), free_vars, outer_lets'
             in
             ( ( Environment.add var shadowed
               , free_vars
               , outer_lets' :: outer_lets )
             , {var; iterator} ))
          (Environment.empty, Environment.empty, [])
          bindings
      in
      let outer_lets =
        let rec rev_rev_concat acc = function
          | []        -> acc
          | xs :: xss -> rev_rev_concat (List.rev_append xs acc) xss
        in rev_rev_concat [] outer_lets
      in
      outer_lets, bindings

    (* Python doesn't shadow variables which have the same name, it reuses the
       same mutable cell.  Thus, in the Python list comprehension
       [[a for a in [0] for _ in [0, 0] for a in [a, 1]]], the second [a]
       clobbers the first, and the result is [[0, 1, 1, 1]] instead of (as it
       would be in OCaml or Haskell) [[0, 1, 0, 1]].  To avoid this, we make
       every variable in a Python comprehension unique; the above comprehension
       would become [[a for a2 in [0] for _ in [0, 0] for a in [a2, 1]]]. *)
    module Make_all_variables_unique = struct
      module Rename = struct
        let var renaming x =
          Option.value ~default:x (Var.Map.find_opt x renaming)

        let int_term renaming = function
          | Literal  n -> Literal n
          | Variable x -> Variable (var renaming x)

        let iterator renaming = function
          | Range { start; direction; stop } ->
              Range { start     = int_term renaming start
                    ; direction
                    ; stop      = int_term renaming stop }
          | Sequence seq ->
              Sequence (List.map (int_term renaming) seq)
      end

      let duplicate_bindings clauses =
        let merge_counts f =
          List.fold_left
            (fun m x -> Var.Map.union (fun _ n1 n2 -> Some (n1 + n2)) (f x) m)
            Var.Map.empty
        in
        Var.Map.filter
          (fun _ n -> n > 1)
          (merge_counts
             (function
               | For bs ->
                   merge_counts (fun {var; _} -> Var.Map.singleton var 1) bs
               | When _ ->
                   Var.Map.empty)
             clauses)

      let bindings dups renaming =
        List.fold_left_map
          (fun (dups, renaming') {var; iterator} ->
            let iterator = Rename.iterator renaming iterator in
            match Var.Map.find_opt var dups with
            | Some n ->
                let var' = var ^ Int.to_string n in
                let renaming' = Var.Map.add var var' renaming' in
                let dups =
                  Var.Map.update
                    var
                    (function
                      | Some 2 -> None
                      | Some n -> Some (n-1)
                      | None   -> assert false)
                    dups
                in
                (dups, renaming'), {var = var'; iterator}
            | None ->
                (dups, Var.Map.remove var renaming'), {var; iterator})
          (dups, renaming)

      let clauses cs =
        cs |>
        List.fold_left_map
          (fun ((dups, renaming) as acc) -> function
             | For bs ->
                 let (dups, renaming), bs = bindings dups renaming bs in
                 (dups, renaming), For bs
             | When(pred, x) ->
                 acc, When(pred, Rename.var renaming x))
          (duplicate_bindings cs, Var.Map.empty) |>
        snd
    end

    let clause o = function
      | For bindings ->
          let intro, sep, (extra_clauses, bindings) =
            match o with
            | OCaml _ ->
                ["for"], " and ", ([], bindings)
            | Haskell ->
                [],
                ", ",
                protect_parallel_bindings
                  (fun x e -> tokens ["let"; x; "="; e])
                  bindings
            | Python ->
                (* [Make_all_variables_unique] has already been applied, so we
                   don't need to call [protect_parallel_bindings] *)
                ["for"], " for ", ([], bindings)
          in
          comprehension_clauses o
            (extra_clauses @
             intro @
             [bindings |> List.map (binding o) |> String.concat sep])
      | When(pred, x) ->
          let kwd = match o with
            | OCaml _ -> ["when"]
            | Haskell -> []
            | Python  -> ["if"]
          in
          let pred_pre, pred_post = predicate o pred in
          tokens (kwd @ pred_pre @ (x :: pred_post))

    let comprehension o {env; clauses} =
      let clauses = match o with
        | OCaml _ | Haskell -> clauses
        | Python            -> Make_all_variables_unique.clauses clauses
      in
      let body    = tuple (Environment.variables env) in
      let clauses = comprehension_clauses o (List.map (clause o) clauses) in
      let sep     = match o with
        | OCaml _ | Python -> " "
        | Haskell          -> " | "
      in
      sequence o (body ^ sep ^ clauses)
  end

  let generator = Generator.comprehension
  let shrink    = Shrink.comprehension
  let to_string = To_string.comprehension
end

module Interactive_command = struct
  let command cmd args ~setup ~input ~output ~f =
    let inch, outch =
      Unix.open_process_args cmd (Array.of_list (cmd :: args))
    in
    let output str = Util.output_line outch (output str) in
    let interact str =
      output str;
      input inch
    in
    let cleanup () = ignore (Unix.close_process (inch, outch)) in
    match setup output; f interact with
    | result      -> cleanup (); result
    | exception e -> cleanup (); raise e

  (* We need to read every comprehension's output in a character-wise identical
     way.  We settle on Python's list syntax: square brackets (no pipes),
     comma-separated, with spaces after all the commas.  This choice is because
     (1) it's easier to replace all of OCaml's semicolons with commas than it it
     is to replace *some* of Haskell/Python's commas with semicolons; and (2) it
     looks nicer to have spaces after commas (like Python, as well as OCaml)
     than to not do so (like Haskell).  *)

  (* This custom printer is necessary because long lists cause the default
     printer to stack overflow.  Since we're writing our own, we use commas as a
     separator here, a la Python, rather than relying on the substitution later.
     (We do still have to substitute later, though, for arrays.) *)
  let ocaml_code_pp_list_as_python = {|
    let pp_list pp_elt fmt xs =
      let buf = Buffer.create 256 in
      let rec fill_buf prefix = function
        | x :: xs ->
            let fbuf = Format.formatter_of_buffer buf in
            Format.pp_set_max_indent fbuf Int.max_int;
            Buffer.add_string buf prefix;
            Format.fprintf fbuf "%a%!" pp_elt x;
            fill_buf ", " xs
        | [] ->
            ();
      in
      Buffer.add_char buf '[';
      fill_buf "" xs;
      Buffer.add_char buf ']';
      Format.fprintf fmt "%s" (Buffer.contents buf)
    |}

  let input_ocaml_list_or_array_as_python_list i =
    let input = Buffer.create 16 in
    let rec input_lines () =
      let line = input_line i in
      Buffer.add_string input line;
      if not (String.contains line ']') then input_lines ()
    in
    input_lines ();
    let raw_list = Buffer.contents input in
    let start = String.index  raw_list '[' in
    let stop  = String.rindex raw_list ']' in
    let list  = String.sub raw_list start (stop - start + 1) in
    list
    |> Str.global_replace (Str.regexp "[ \n]+")  " "
    |> Str.global_replace (Str.regexp "\\[[|:]") "["
    |> Str.global_replace (Str.regexp "[|:]\\]") "]"
    |> Str.global_replace (Str.regexp ";")       ","

  let input_haskell_list_as_python_list i =
    i |> input_line |> Str.global_replace (Str.regexp ",") ", "

  let ocaml ~f =
    command
      "../../../ocaml"
      [ "-extension"; "comprehensions"
      ; "-noprompt"; "-no-version"
      ; "-w"; "no-unused-var" ]
      ~setup:(fun output ->
        output ("#print_length " ^ Int.to_string Int.max_int);
        output ocaml_code_pp_list_as_python;
        output "#install_printer pp_list")
      ~input:input_ocaml_list_or_array_as_python_list
      ~output:(fun str -> str ^ ";;")
      ~f

  (* If GHCi isn't on a tty, it doesn't display a prompt, AFAICT *)
  let haskell ~f =
    command
      "/usr/bin/ghci"
      ["-v0"; "-ignore-dot-ghci"]
      ~setup:(Fun.const ())
      ~input:input_haskell_list_as_python_list
      ~output:Fun.id
      ~f

  let python ~f =
    command
      "/usr/bin/python3"
      ["-qic"; "import sys\nsys.ps1 = ''"]
      ~setup:(Fun.const ())
      ~input:input_line
      ~output:Fun.id
      ~f
end

module Log_test_cases = struct
  let to_file file f =
    let oc = open_out file in
    try
      output_string oc
{|(* TEST
   flags = "-extension-universe beta"
   * expect
*)

(* Generated by quickcheck_lists_arrays_haskell_python.ml; filtered down to all
   tests of reasonable size; and reflowed to fit in 80-character lines. *)

(* NOTE: If you're saving these tests, don't forget to make those last two
   changes! *)|};
      let r = f (Printf.fprintf oc "\n\n%s;;\n[%%%%expect{||}];;%!") in
      output_char oc '\n';
      close_out oc;
      r
    with
    | exn ->
        close_out_noerr oc;
        raise exn
end

module Main = struct
  type output = { ocaml_list            : string
                ; ocaml_mutable_array   : string
                ; ocaml_immutable_array : string
                ; haskell               : string
                ; python                : string }

  let output_for o output =
    match (o : Comprehension.To_string.format) with
    | OCaml List            -> output.ocaml_list
    | OCaml Mutable_array   -> output.ocaml_mutable_array
    | OCaml Immutable_array -> output.ocaml_immutable_array
    | Haskell               -> output.haskell
    | Python                -> output.python

  let print_counterexample oc counterexample data =
    let printf format_string = Printf.fprintf oc format_string in
    let output_for, printf_for_data = match data with
      | Some data -> (fun o -> output_for o data), printf
      | None      -> (fun _ -> ""),                Printf.ifprintf oc
    in
    let print_comprehension tag align o =
      let counterexample_str = Comprehension.to_string o counterexample in
      let indent = String.make (String.length tag) ' ' in
      printf          "  %s:%s %s\n"      tag    align counterexample_str;
      printf_for_data "  %s %s   = %s\n"  indent align (output_for o)
    in
    print_comprehension "OCaml list" "  " (OCaml List);
    print_comprehension "OCaml array" " " (OCaml Mutable_array);
    print_comprehension "OCaml iarray" "" (OCaml Immutable_array);
    print_comprehension "Haskell" "     " Haskell;
    print_comprehension "Python" "      " Python

  let different_comprehensions_agree ?seed ?output max_tests =
    let ( = ) = String.equal in
    Interactive_command.ocaml   ~f:(fun ocaml ->
    Interactive_command.haskell ~f:(fun haskell ->
    Interactive_command.python  ~f:(fun python ->
    Log_test_cases.to_file "comprehensions_from_quickcheck-log.ml" (fun log ->
      let ocaml comp = log comp; ocaml comp in
      QuickCheck.main
        ?seed ?output
        max_tests
        Comprehension.generator Comprehension.shrink
        print_counterexample
        (fun c ->
           let run repl fmt = repl (Comprehension.to_string fmt c) in
           let ocaml_list            = run ocaml   (OCaml List)            in
           let ocaml_mutable_array   = run ocaml   (OCaml Mutable_array)   in
           let ocaml_immutable_array = run ocaml   (OCaml Immutable_array) in
           let haskell               = run haskell Haskell                 in
           let python                = run python  Python                  in
           if ocaml_list            = ocaml_mutable_array   &&
              ocaml_mutable_array   = ocaml_immutable_array &&
              ocaml_immutable_array = haskell               &&
              haskell               = python
           then OK
           else Failed_with
                  { ocaml_list
                  ; ocaml_mutable_array
                  ; ocaml_immutable_array
                  ; haskell
                  ; python })))))
end

let () = Main.different_comprehensions_agree 1_000
