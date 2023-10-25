module type WS = sig
  type t

  val join : t -> t -> t

  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit
end

module Make (Witnesses : WS) = struct
  (** Abstract value for each component of the domain. *)
  module V = struct
    type t =
      | Top of Witnesses.t
      | Safe
      | Bot

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
      | Top _, Top _ -> true
      | Bot, Safe -> true
      | Bot, Top _ -> true
      | Safe, Top _ -> true
      | Top _, (Bot | Safe) -> false
      | Safe, Bot -> false

    let is_not_safe = function Top _ -> true | Safe | Bot -> false

    let print ~witnesses ppf = function
      | Bot -> Format.fprintf ppf "bot"
      | Top w ->
        Format.fprintf ppf "top";
        if witnesses then Format.fprintf ppf " (%a)" Witnesses.print w
      | Safe -> Format.fprintf ppf "safe"
  end

  module Value = struct
    (** Lifts V to triples  *)
    type t =
      { nor : V.t;
        exn : V.t;
        div : V.t
      }

    let bot = { nor = V.Bot; exn = V.Bot; div = V.Bot }

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

    let normal_return = { bot with nor = V.Safe }

    let exn_escape = { bot with exn = V.Safe }

    let diverges = { bot with div = V.Safe }

    let safe = { nor = V.Safe; exn = V.Safe; div = V.Safe }

    let top w = { nor = V.Top w; exn = V.Top w; div = V.Top w }

    let relaxed w = { nor = V.Safe; exn = V.Top w; div = V.Top w }

    let print ~witnesses ppf { nor; exn; div } =
      let pp = V.print ~witnesses in
      Format.fprintf ppf "{ nor=%a; exn=%a; div=%a }" pp nor pp exn pp div
  end
end
