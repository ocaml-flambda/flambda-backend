let[@inline never] dummy_float () = Sys.opaque_identity 0.

let[@inline] bar flag a b c d e f g h i j k l m n o p q r s t =
  let[@inline never] gc () = Gc.full_major () in
  let[@inline never] foo a b c d e f g h i j k l m n o p q r s t =
    gc (); a+a
  in
  let [@inline never] hide x = x in
  let[@inline never][@local never] get_uu flag a b c d e f g h i j k l m n o p q r s t =
    let a = Sys.opaque_identity a in
    let b = Sys.opaque_identity b in
    let c = Sys.opaque_identity c in
    let d = Sys.opaque_identity d in
    let e = Sys.opaque_identity e in
    let f = Sys.opaque_identity f in
    let g = Sys.opaque_identity g in
    let h = Sys.opaque_identity h in
    let i = Sys.opaque_identity i in
    let j = Sys.opaque_identity j in
    let k = Sys.opaque_identity k in
    let l = Sys.opaque_identity l in
    let m = Sys.opaque_identity m in
    let n = Sys.opaque_identity n in
    let o = Sys.opaque_identity o in
    let p = Sys.opaque_identity p in
    let q = Sys.opaque_identity q in
    let r = Sys.opaque_identity r in
    let s = Sys.opaque_identity s in
    let t = Sys.opaque_identity t in
    let z = dummy_float () in
    let vv1 = String.make 128 '\000' in
    let[@inline available][@local never] uu = fun () -> let [@inline never][@local never] call_foo b c d e f g h i j k l m n o p q r s t  = Gc.full_major (); foo a b c d e f g h i j k l m n o p q r s t in if flag then (a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a) else call_foo b c d e f g h i j k l m n o p q r s t in
    let vv2 = String.make 129 '\000' in
    (vv1, uu, vv2)
  in
  let vv1, uu, vv2 = get_uu flag a b c d e f g h i j k l m n o p q r s t in
  (vv1, uu (), vv2)