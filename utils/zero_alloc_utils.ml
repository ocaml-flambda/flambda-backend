(* The meaning of keywords [strict] and [never_returns_normally] is defined in
   terms of abstract values as follows:

   relaxed (default): nor = Safe and exn = Top and div = Top strict: nor = Safe
   and exn = Safe and div = Safe never_returns_normally: nor = Bot and exn = Top
   and div = Top

   where [nor] means normal return of the call, [exn] means return via an
   exception, [div] means diverging (non-terminating) executions, and the
   meaning and order of elements is:

   Top may allocate Safe does not allocate on any execution paths Bot
   unreachable

   Using more than one keyword means intersection (i.e., meet of the elements,
   pointwise lifted to tuples), so we get the following:

   [@zero_alloc assume] nor = Safe and exn = Top and div = Top [@zero_alloc
   assume strict] nor = Safe and exn = Safe and div = Safe [@zero_alloc assume
   strict never_returns_normally] nor = Bot and exn = Safe and div = Safe
   [@zero_alloc assume never_returns_normally] nor = Bot and exn = Top and div =
   Top

   See [Value] and [Annotation] in [backend/checkmach.ml]. *)
(* CR gyorsh: should we move [Value] and [Annotation] here or maybe "utils" and
   use them directly, instead of the weird compare function that abstracts them?
   Perhaps we should translate "strict" and "never_returns_normally" directly
   into (nor,exn,div) *)

module type WS = sig
  type t

  val empty : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module type Component = sig
  type t

  type witnesses

  val top : witnesses -> t

  val safe : t

  val bot : t

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val compare : t -> t -> int

  val print : witnesses:bool -> Format.formatter -> t -> unit
end

module Make_component (Witnesses : WS) = struct
  (* keep in sync with "resolved" values in Checkmach. *)
  type t =
    | Top of Witnesses.t
    | Safe
    | Bot

  let bot = Bot

  let top w = Top w

  let safe = Safe

  let join c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.join w1 w2)
    | Safe, Bot | Bot, Safe -> Safe
    | Top w1, Bot | Top w1, Safe | Bot, Top w1 | Safe, Top w1 -> Top w1

  let meet c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top w1, Top w2 -> Top (Witnesses.meet w1 w2)
    | Safe, Bot | Bot, Safe -> Bot
    | Top _, Bot | Bot, Top _ -> Bot
    | Top _, Safe | Safe, Top _ -> Safe

  let lessequal v1 v2 =
    match v1, v2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top w1, Top w2 -> Witnesses.lessequal w1 w2
    | Bot, Safe -> true
    | Bot, Top _ -> true
    | Safe, Top _ -> true
    | Top _, (Bot | Safe) -> false
    | Safe, Bot -> false

  let compare t1 t2 =
    match t1, t2 with
    | Bot, Bot -> 0
    | Safe, Safe -> 0
    | Top w1, Top w2 -> Witnesses.compare w1 w2
    | Bot, (Safe | Top _) -> -1
    | (Safe | Top _), Bot -> 1
    | Safe, Top _ -> -1
    | Top _, Safe -> 1

  let print ~witnesses ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Top w ->
      Format.fprintf ppf "top";
      if witnesses then Format.fprintf ppf " (%a)" Witnesses.print w
    | Safe -> Format.fprintf ppf "safe"
end

module Make_value
    (Witnesses : WS)
    (V : Component with type witnesses := Witnesses.t) =
struct
  (** Lifts V to triples  *)
  type t =
    { nor : V.t;
      exn : V.t;
      div : V.t
    }

  let bot = { nor = V.bot; exn = V.bot; div = V.bot }

  let lessequal v1 v2 =
    V.lessequal v1.nor v2.nor && V.lessequal v1.exn v2.exn
    && V.lessequal v1.div v2.div

  let join v1 v2 =
    { nor = V.join v1.nor v2.nor;
      exn = V.join v1.exn v2.exn;
      div = V.join v1.div v2.div
    }

  let meet v1 v2 =
    { nor = V.meet v1.nor v2.nor;
      exn = V.meet v1.exn v2.exn;
      div = V.meet v1.div v2.div
    }

  let normal_return = { bot with nor = V.safe }

  let exn_escape = { bot with exn = V.safe }

  let diverges = { bot with div = V.safe }

  let safe = { nor = V.safe; exn = V.safe; div = V.safe }

  let top w = { nor = V.top w; exn = V.top w; div = V.top w }

  let relaxed w = { nor = V.safe; exn = V.top w; div = V.top w }

  let of_annotation ~strict ~never_returns_normally ~never_raises =
    let res = if strict then safe else relaxed Witnesses.empty in
    let res = if never_raises then { res with exn = V.bot } else res in
    if never_returns_normally then { res with nor = V.bot } else res

  let print ~witnesses ppf { nor; exn; div } =
    let pp = V.print ~witnesses in
    Format.fprintf ppf "{ nor=%a;@ exn=%a;@ div=%a }@," pp nor pp exn pp div

  let compare { nor = n1; exn = e1; div = d1 } { nor = n2; exn = e2; div = d2 }
      =
    let c = V.compare n1 n2 in
    if c <> 0
    then c
    else
      let c = V.compare e1 e2 in
      if c <> 0 then c else V.compare d1 d2
end

module Assume_info = struct
  module Witnesses = struct
    type t = unit

    let join _ _ = ()

    let lessequal _ _ = true

    let meet _ _ = ()

    let print _ _ = ()

    let empty = ()

    let compare _ _ = 0
  end

  module V = Make_component (Witnesses)
  module Value = Make_value (Witnesses) (V)

  type t =
    | No_assume
    | Assume of Value.t
    | Assume_inferred of Value.t
  (* CR ccasinghino: consider extending this time to also capture "check"
     attributes, and using it everywhere in typed tree instead of sometimes
     having a check_attribute and sometimes having this type. *)

  let compare t1 t2 =
    match t1, t2 with
    | No_assume, No_assume -> 0
    | Assume v1, Assume v2 -> Value.compare v1 v2
    | Assume_inferred v1, Assume_inferred v2 -> Value.compare v1 v2
    | No_assume, (Assume _ | Assume_inferred _) -> -1
    | Assume_inferred _, Assume _ -> -1
    | Assume _, Assume_inferred _ -> 1
    | (Assume _ | Assume_inferred _), No_assume -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let print ppf = function
    | No_assume -> ()
    | Assume v -> Format.fprintf ppf "%a" (Value.print ~witnesses:false) v
    | Assume_inferred v ->
      Format.fprintf ppf "(inferred)%a" (Value.print ~witnesses:false) v

  let to_string v = Format.asprintf "%a" print v

  let join t1 t2 =
    match t1, t2 with
    | No_assume, No_assume -> No_assume
    | No_assume, (Assume _ | Assume_inferred _)
    | (Assume _ | Assume_inferred _), No_assume ->
      No_assume
    | Assume t1, Assume t2 -> Assume (Value.join t1 t2)
    | Assume_inferred t1, Assume_inferred t2 ->
      Assume_inferred (Value.join t1 t2)
    | Assume t1, Assume_inferred t2 | Assume_inferred t1, Assume t2 ->
      Assume_inferred (Value.join t1 t2)

  let meet t1 t2 =
    match t1, t2 with
    | No_assume, No_assume -> No_assume
    | No_assume, (Assume _ as t) | (Assume _ as t), No_assume -> t
    | No_assume, (Assume_inferred _ as t) | (Assume_inferred _ as t), No_assume
      ->
      t
    | Assume t1, Assume t2 -> Assume (Value.meet t1 t2)
    | Assume_inferred t1, Assume_inferred t2 ->
      Assume_inferred (Value.meet t1 t2)
    | Assume t1, Assume_inferred t2 -> Assume (Value.meet t1 t2)
    | Assume_inferred t1, Assume t2 -> Assume (Value.meet t1 t2)

  let none = No_assume

  let create ~strict ~never_returns_normally ~never_raises ~inferred =
    let v = Value.of_annotation ~strict ~never_returns_normally ~never_raises in
    match inferred with false -> Assume v | true -> Assume_inferred v

  let get_value t =
    match t with No_assume -> None | Assume v | Assume_inferred v -> Some v

  let is_none t =
    match t with No_assume -> true | Assume _ | Assume_inferred _ -> false

  let is_inferred t =
    match t with Assume_inferred _ -> true | Assume _ | No_assume -> false
end
