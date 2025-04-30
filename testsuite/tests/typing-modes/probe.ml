(* TEST
    expect;
*)

(* probe can refer to local,nonportable,once values *)

let f (x @ local nonportable once) =
    [%probe "a" (let _ = x in ())]
[%%expect{|
val f : 'a @ local once -> unit = <fun>
|}]
