(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_uncontended_vv : ('a : value & value) @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_portable_vv : ('a : value & value) @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_uncontended_vv : ('a : value & value). 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_portable_vv : ('a : value & value). 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
|}]

type 'a portable : value mod portable = { portable : 'a @@ portable } [@@unboxed]
type 'a portable_vv : value & value mod portable = { portable : ('a : value & value) @@ portable } [@@unboxed]
type 'a contended : value mod contended = { contended : 'a @@ contended } [@@unboxed]
type 'a contended_vv : value & value mod contended = { contended : ('a : value & value) @@ contended } [@@unboxed]
type 'a portended : value mod portable contended = { portended : 'a @@ portable contended } [@@unboxed]
type 'a portended_vv : value & value mod portable contended = { portended : ('a : value & value) @@ portable contended } [@@unboxed]
[%%expect{|
type 'a portable = { portable : 'a @@ portable; } [@@unboxed]
type ('a : value & value) portable_vv = { portable : 'a @@ portable; } [@@unboxed]
type 'a contended = { contended : 'a @@ contended; } [@@unboxed]
type ('a : value & value) contended_vv = { contended : 'a @@ contended; } [@@unboxed]
type 'a portended = { portended : 'a @@ portable contended; } [@@unboxed]
type ('a : value & value) portended_vv = {
  portended : 'a @@ portable contended;
} [@@unboxed]
|}]

(* Unboxed records with modalities should cross regardless of what we put in them *)
let foo (x : (int -> int) portable @@ nonportable) = use_portable x
let foo (x : (int -> int) ref portable @@ nonportable) = use_portable x
let foo (x : int ref contended @@ contended) = use_uncontended x
let foo (x : (int -> int) ref contended @@ contended) = use_uncontended x
let foo (x : (int -> int) ref portended @@ nonportable) =
  use_uncontended x;
  use_portable x
let foo (x : (int -> int) ref ref portended @@ nonportable) =
  use_uncontended x;
  use_portable x
type test = (int -> int) portable require_portable
type test = (int -> int) ref portable require_portable
type test = int ref contended require_contended
type test = (int -> int) ref contended require_contended
type test = (int -> int) ref portended require_contended
type test = (int -> int) ref portended require_portable
type test = (int -> int) ref ref portended require_contended
type test = (int -> int) ref ref portended require_portable
[%%expect{|
val foo : (int -> int) portable -> unit = <fun>
val foo : (int -> int) ref portable -> unit = <fun>
val foo : int ref contended @ contended -> unit = <fun>
val foo : (int -> int) ref contended @ contended -> unit = <fun>
val foo : (int -> int) ref portended -> unit = <fun>
val foo : (int -> int) ref ref portended -> unit = <fun>
type test = (int -> int) portable require_portable
type test = (int -> int) ref portable require_portable
type test = int ref contended require_contended
type test = (int -> int) ref contended require_contended
type test = (int -> int) ref portended require_contended
type test = (int -> int) ref portended require_portable
type test = (int -> int) ref ref portended require_contended
type test = (int -> int) ref ref portended require_portable
|}]

(* Modalities should propagate through arbitrary levels of nesting *)
let foo (x : ((int -> int) portable portable portable) @@ nonportable) = use_portable x
let foo (x : ((int -> int) portable ref portable ref portable) @@ nonportable) = use_portable x
let foo (x : (((int -> int) ref) portable contended portable contended) @@ nonportable contended) =
  use_uncontended x;
  use_portable x

type test = ((int -> int) portable portable portable) require_portable
type test = ((int -> int) portable ref portable ref portable) require_portable
type test = (((int -> int) ref) portable contended portable contended) require_portable
type test = (((int -> int) ref) portable contended portable contended) require_contended
[%%expect{|
val foo : (int -> int) portable portable portable -> unit = <fun>
val foo : (int -> int) portable ref portable ref portable -> unit = <fun>
val foo :
  (int -> int) ref portable contended portable contended @ contended -> unit =
  <fun>
type test = (int -> int) portable portable portable require_portable
type test = (int -> int) portable ref portable ref portable require_portable
type test =
    (int -> int) ref portable contended portable contended require_portable
type test =
    (int -> int) ref portable contended portable contended require_contended
|}]

(* Modalities should work in unboxed tuples and products too *)
let foo (x : #(int ref contended * int ref contended) @@ contended) = use_uncontended_vv x
let foo (x : #(int ref * int ref) contended_vv @@ contended) = use_uncontended_vv x
let foo (x : #(int ref contended * int ref contended) contended_vv @@ contended) = use_uncontended_vv x
let foo (x : #((int -> int) ref portended * (int -> int) ref portended) @@ nonportable contended) =
  use_portable_vv x;
  use_uncontended_vv x
let foo (x : #((int -> int) portable * int ref contended) portended_vv @@ contended) =
  use_portable_vv x;
  use_uncontended_vv x
[%%expect{|
val foo : #(int ref contended * int ref contended) @ contended -> unit =
  <fun>
val foo : #(int ref * int ref) contended_vv @ contended -> unit = <fun>
val foo :
  #(int ref contended * int ref contended) contended_vv @ contended -> unit =
  <fun>
val foo :
  #((int -> int) ref portended * (int -> int) ref portended) @ contended ->
  unit = <fun>
val foo :
  #((int -> int) portable * int ref contended) portended_vv @ contended ->
  unit = <fun>
|}]

(* [@@unboxed] variants, of all sorts *)
type 'a portable = Portable of 'a @@ portable [@@unboxed]
let foo (x : (int -> int) portable @@ nonportable) = use_portable x
[%%expect{|
type 'a portable = Portable of 'a @@ portable [@@unboxed]
val foo : (int -> int) portable -> unit = <fun>
|}]

let foo (x : int ref portable @@ contended) = use_uncontended x
[%%expect{|
Line 1, characters 62-63:
1 | let foo (x : int ref portable @@ contended) = use_uncontended x
                                                                  ^
Error: This value is "contended" but expected to be "uncontended".
|}]

type 'a portable = Portable of { portable : 'a @@ portable } [@@unboxed]
let foo (x : (int -> int) portable @@ nonportable) = use_portable x
[%%expect{|
type 'a portable = Portable of { portable : 'a @@ portable; } [@@unboxed]
val foo : (int -> int) portable -> unit = <fun>
|}]

let foo (x : int ref portable @@ contended) = use_uncontended x
[%%expect{|
Line 1, characters 62-63:
1 | let foo (x : int ref portable @@ contended) = use_uncontended x
                                                                  ^
Error: This value is "contended" but expected to be "uncontended".
|}]

type 'a portable = Portable : 'a @@ portable -> 'a portable [@@unboxed]
let foo (x : (int -> int) portable @@ nonportable) = use_portable x
[%%expect{|
type 'a portable = Portable : 'a @@ portable -> 'a portable [@@unboxed]
val foo : (int -> int) portable -> unit = <fun>
|}]

let foo (x : int ref portable @@ contended) = use_uncontended x
[%%expect{|
Line 1, characters 62-63:
1 | let foo (x : int ref portable @@ contended) = use_uncontended x
                                                                  ^
Error: This value is "contended" but expected to be "uncontended".
|}]

type 'a portable = Portable : { portable : 'a @@ portable } -> 'a portable [@@unboxed]
let foo (x : (int -> int) portable @@ nonportable) = use_portable x
[%%expect{|
type 'a portable = Portable : { portable : 'a @@ portable; } -> 'a portable [@@unboxed]
val foo : (int -> int) portable -> unit = <fun>
|}]

let foo (x : int ref portable @@ contended) = use_uncontended x
[%%expect{|
Line 1, characters 62-63:
1 | let foo (x : int ref portable @@ contended) = use_uncontended x
                                                                  ^
Error: This value is "contended" but expected to be "uncontended".
|}]
