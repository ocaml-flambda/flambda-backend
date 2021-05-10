(** An efficient sampler for geometrically distributed random variables.
    (Port to OCaml of the statmemprof sampler used in the OCaml runtime)

    A sampler is parameterised by a sampling_rate λ, and simulates a long
    sequence of flips of a biased coin that has probability λ of coming up
    heads.

    The result of [draw] is an integer distributed as the length of the
    gaps between two successive heads. This is a geometrically distributed
    random variable: the expected value is 1/λ, and the probability that
    draw returns k is (1-λ)^(k-1) * λ (that is, the probability of (k-1)
    tails followed by a heads).

    NB: Note that here, we're adopting the convention that two consecutive
    heads counts as a gap of length 1. In other words, this distribution is
    the number of times you need to flip the coin to see heads, rather than
    the number of tails you'll see while doing so (which is one less).
    This means that the result is never zero.  This convention is not
    universal, and some authers use "geometrically distributed" for the
    zero-based distribution. *)
type t

(** Create a sampler with a given sampling rate and randomness source.
    The default for [rand] uses a constant seed, giving deterministic results *)
val make : ?rand:Random.State.t -> sampling_rate:float -> unit -> t

(** Returns a geometrically-distributed random integer in the range [1..inf)
    with mean 1/sampling_rate *)
val draw : t -> int
