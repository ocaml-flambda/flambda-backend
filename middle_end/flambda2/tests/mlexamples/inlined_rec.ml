external ( - ) : int -> int -> int = "%subint"

external ( * ) : int -> int -> int = "%mulint"

let[@inline] apply ~f i = (f [@inlined hint]) i

let rec fact n =
  match n with
  | 0 -> 1
  | _ ->
    (* If [@inlined] isn't careful, this recursive call will get unfolded no
       matter what the max rec depth is set to. In more complicated cases in the
       wild, this has led to infinite unfolding. *)
    n * apply ~f:fact (n - 1)

let i = fact 1_000_000
