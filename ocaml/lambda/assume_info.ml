(*

   The meaning of keywords [strict] and [never_returns_normally]
   is defined in terms of abstract values as follows:

   relaxed (default):         nor = Safe and exn = Top  and div = Top
   strict:                    nor = Safe and exn = Safe and div = Safe
   never_returns_normally:    nor = Bot  and exn = Top  and div = Top

   where [nor] means normal return of the call, [exn] means return via an exception,
   [div] means diverging (non-terminating) executions,
   and the meaning and order of elements is:

   Top    may allocate
   Safe   does not allocate on any execution paths
   Bot    unreachable

   Using more than one keyword means intersection (i.e., meet of the  elements,
   pointwise lifted to tuples), so we get the following:

   [@zero_alloc assume]                               nor = Safe and exn = Top  and div = Top
   [@zero_alloc assume strict]                        nor = Safe and exn = Safe and div = Safe
   [@zero_alloc assume strict never_returns_normally] nor = Bot  and exn = Safe and div = Safe
   [@zero_alloc assume never_returns_normally]        nor = Bot  and exn = Top  and div = Top

   See [Value] and [Annotation] in [backend/checkmach.ml].
*)
(* CR gyorsh: should we move [Value] and [Annotation] here or maybe "utils" and use them
   directly, instead of the weird compare function that abstracts them? Perhaps we
   should translate "strict" and "never_returns_normally" directly into (nor,exn,div)
*)

module Witnesses = struct
  type t = unit

  let join _ _ = ()
  let meet _ _ = ()
  let print _ _ = ()
  let empty = ()
  let compare _ _ = 0
end

include Zero_alloc_utils.Make (Witnesses)

type t = No_assume | Assume of Value.t

let compare t1 t2 =
  match (t1, t2) with
  | No_assume, No_assume -> 0
  | Assume v1, Assume v2 -> Value.compare v1 v2
  | No_assume, Assume _ -> -1
  | Assume _, No_assume -> 1

let equal t1 t2 = compare t1 t2 = 0

let print ppf = function
  | No_assume -> ()
  | Assume v -> Format.fprintf ppf "%a" (Value.print ~witnesses:false) v

let to_string v = Format.asprintf "%a" print v

let join t1 t2 =
  match (t1, t2) with
  | No_assume, No_assume -> No_assume
  | No_assume, Assume _
  | Assume _, No_assume ->
      No_assume
  | Assume t1, Assume t2 -> Assume (Value.join t1 t2)

let meet t1 t2 =
  match (t1, t2) with
  | No_assume, No_assume -> No_assume
  | No_assume, (Assume _ as t)
  | (Assume _ as t), No_assume ->
      t
  | Assume t1, Assume t2 -> Assume (Value.meet t1 t2)

let none = No_assume

let create ~strict ~never_returns_normally =
  let res =
    if strict then
      Value.safe
    else
      Value.relaxed Witnesses.empty
  in
  let res =
    if never_returns_normally then
      { res with nor = V.Bot }
    else
      res
  in
  Assume res

let get_value t =
  match t with
  | No_assume -> None
  | Assume v -> Some v

let is_none t =
  match t with
  | No_assume -> true
  | Assume _ -> false
