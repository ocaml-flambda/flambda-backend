(* TEST
   * expect *)
type t [@@deprecated]
[%%expect{|
type t
|}]

type warn_no_local = t -> unit
[%%expect{|
Line 1, characters 21-22:
1 | type warn_no_local = t -> unit
                         ^
Alert deprecated: t
type warn_no_local = t -> unit
|}]

type warn_local = local_ t -> unit
[%%expect{|
Line 1, characters 25-26:
1 | type warn_local = local_ t -> unit
                             ^
Alert deprecated: t
type warn_local = local_ t -> unit
|}]

type quiet_no_local = (t[@alert "-deprecated"]) -> unit
[%%expect{|
type quiet_no_local = t -> unit
|}]

type quiet_local = local_ (t[@alert "-deprecated"]) -> unit
[%%expect{|
type quiet_local = local_ t -> unit
|}]
