(* TEST

 flags = "-dlambda -w -unused-var -dno-unique-ids"
 * expect

*)

(* This test prints the translation of functions whose arity exceeds
   the max arity allowed by the native code backend (126, at time of writing).
   It has a particular focus on functions that involve locals, either in
   parameter or in return position.

   The point of this test is to ensure that we're not unintentionally changing
   the various mode-related fields in [Lambda.function]: parameter modes,
   region, and nlocal. It's fine to accept changes that don't affect these
   fields or that affect these fields in understandable ways, but please make
   sure that the comments on the tests stay up-to-date even when the output
   changes.
 *)

(* The functions themselves are globally allocated *)

(* No local arguments *)
let no_local_params
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 = ()
;;

[%%expect {|
(let
  (no_local_params =
     (function {nlocal = 0} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130 x131 : int 0))
  (apply (field_imm 1 (global Toploop!)) "no_local_params" no_local_params))
val no_local_params :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 -> x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> unit = <fun>
|}]

(* No local arguments; local returning *)
let no_local_params__local_returning
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x2)
;;

[%%expect {|
(let
  (no_local_params__local_returning =
     (function {nlocal = 1} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130 x131
       [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x2)))
  (apply (field_imm 1 (global Toploop!)) "no_local_params__local_returning"
    no_local_params__local_returning))
val no_local_params__local_returning :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 -> x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> local_ 'a * 'b =
  <fun>
|}]

(* first local argument comes after the split point *)
let local_param_after_split
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~(local_ x130) ~x131 = ()
;;

[%%expect {|
(let
  (local_param_after_split =
     (function {nlocal = 2} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130[L] x131 : int 0))
  (apply (field_imm 1 (global Toploop!)) "local_param_after_split"
    local_param_after_split))
val local_param_after_split :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 -> x128:'x4 -> x129:'y4 -> x130:local_ 'z4 -> x131:'a5 -> unit =
  <fun>
|}]

(* first local argument comes after the split point; local returning *)
let local_param_after_split__local_returning
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~(local_ x130) ~x131 = local_ (x1, x130)
;;

[%%expect {|
(let
  (local_param_after_split__local_returning =
     (function {nlocal = 2} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130[L] x131
       [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x130)))
  (apply (field_imm 1 (global Toploop!))
    "local_param_after_split__local_returning"
    local_param_after_split__local_returning))
val local_param_after_split__local_returning :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 ->
  x128:'x4 -> x129:'y4 -> x130:local_ 'z4 -> x131:'a5 -> local_ 'a * 'z4 =
  <fun>
|}]

(* first local argument comes immediately after the split point *)
let local_param_just_after_split
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~(local_ x127) ~x128 ~x129 ~x130 ~x131 = ()
;;

[%%expect {|
(let
  (local_param_just_after_split =
     (function {nlocal = 5} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126
       x127[L] x128 x129 x130 x131 : int 0))
  (apply (field_imm 1 (global Toploop!)) "local_param_just_after_split"
    local_param_just_after_split))
val local_param_just_after_split :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:local_ 'w4 -> x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> unit =
  <fun>
|}]

(* first local argument comes immediately after the split point;
   local returning
 *)
let local_param_just_after_split__local_returning
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~(local_ x127) ~x128 ~x129 ~x130 ~x131 = local_ (x1, x127)
;;

[%%expect {|
(let
  (local_param_just_after_split__local_returning =
     (function {nlocal = 5} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126
       x127[L] x128 x129 x130 x131
       [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x127)))
  (apply (field_imm 1 (global Toploop!))
    "local_param_just_after_split__local_returning"
    local_param_just_after_split__local_returning))
val local_param_just_after_split__local_returning :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:local_ 'w4 ->
  x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> local_ 'a * 'w4 = <fun>
|}]

(* first local argument comes immediately before the split point *)
let local_param_just_before_split
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~(local_ x126)
  ~x127 ~x128 ~x129 ~x130 ~x131 = ()
;;

[%%expect {|
(let
  (local_param_just_before_split =
     (function {nlocal = 6} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126[L]
       x127 x128 x129 x130 x131 : int 0))
  (apply (field_imm 1 (global Toploop!)) "local_param_just_before_split"
    local_param_just_before_split))
val local_param_just_before_split :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:local_ 'v4 ->
  x127:'w4 -> x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> unit = <fun>
|}]

(* first local argument comes immediately before the split point;
   local returning
 *)
let local_param_just_before_split__local_returning
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~(local_ x126)
  ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x126)
;;

[%%expect {|
(let
  (local_param_just_before_split__local_returning =
     (function {nlocal = 6} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126[L]
       x127 x128 x129 x130 x131
       [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x126)))
  (apply (field_imm 1 (global Toploop!))
    "local_param_just_before_split__local_returning"
    local_param_just_before_split__local_returning))
val local_param_just_before_split__local_returning :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:local_ 'v4 ->
  x127:'w4 -> x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> local_ 'a * 'v4 =
  <fun>
|}]

(* first local argument comes well before the split point *)
let local_param_before_split
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~(local_ x8) ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 = ()
;;

[%%expect {|
(let
  (local_param_before_split =
     (function {nlocal = 124} x1 x2 x3 x4 x5 x6 x7 x8[L] x9 x10 x11 x12 x13
       x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
       x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
       x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
       x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
       x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
       x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
       x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126
       x127 x128 x129 x130 x131 : int 0))
  (apply (field_imm 1 (global Toploop!)) "local_param_before_split"
    local_param_before_split))
val local_param_before_split :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:local_ 'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 -> x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> unit = <fun>
|}]

(* first local argument comes well before the split point; local returning *)
let local_param_before_split__local_returning
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~(local_ x8) ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x8)
;;

[%%expect {|
(let
  (local_param_before_split__local_returning =
     (function {nlocal = 124} x1 x2 x3 x4 x5 x6 x7 x8[L] x9 x10 x11 x12 x13
       x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
       x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
       x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
       x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
       x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
       x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
       x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126
       x127 x128 x129 x130 x131
       [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x8)))
  (apply (field_imm 1 (global Toploop!))
    "local_param_before_split__local_returning"
    local_param_before_split__local_returning))
val local_param_before_split__local_returning :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:local_ 'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 -> x128:'x4 -> x129:'y4 -> x130:'z4 -> x131:'a5 -> local_ 'a * 'h =
  <fun>
|}]

(* two split points *)
let two_splits
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
  ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
  ~x149 ~x150 ~x151 ~x152 ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
  ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
  ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
  ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
  ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
  ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
  ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
  ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
  ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
  ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
    ()
;;

[%%expect {|
(let
  (two_splits =
     (function {nlocal = 0} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138 x139 x140 x141
       x142 x143 x144 x145 x146 x147 x148 x149 x150 x151 x152 x153 x154 x155
       x156 x157 x158 x159 x160 x161 x162 x163 x164 x165 x166 x167 x168 x169
       x170 x171 x172 x173 x174 x175 x176 x177 x178 x179 x180 x181 x182 x183
       x184 x185 x186 x187 x188 x189 x190 x191 x192 x193 x194 x195 x196 x197
       x198 x199 x200 x201 x202 x203 x204 x205 x206 x207 x208 x209 x210 x211
       x212 x213 x214 x215 x216 x217 x218 x219 x220 x221 x222 x223 x224 x225
       x226 x227 x228 x229 x230 x231 x232 x233 x234 x235 x236 x237 x238 x239
       x240 x241 x242 x243 x244 x245 x246 x247 x248 x249 x250 x251 x252 x253
       x254 x255 x256 x257 x258 : int 0))
  (apply (field_imm 1 (global Toploop!)) "two_splits" two_splits))
val two_splits :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 ->
  x128:'x4 ->
  x129:'y4 ->
  x130:'z4 ->
  x131:'a5 ->
  x132:'b5 ->
  x133:'c5 ->
  x134:'d5 ->
  x135:'e5 ->
  x136:'f5 ->
  x137:'g5 ->
  x138:'h5 ->
  x139:'i5 ->
  x140:'j5 ->
  x141:'k5 ->
  x142:'l5 ->
  x143:'m5 ->
  x144:'n5 ->
  x145:'o5 ->
  x146:'p5 ->
  x147:'q5 ->
  x148:'r5 ->
  x149:'s5 ->
  x150:'t5 ->
  x151:'u5 ->
  x152:'v5 ->
  x153:'w5 ->
  x154:'x5 ->
  x155:'y5 ->
  x156:'z5 ->
  x157:'a6 ->
  x158:'b6 ->
  x159:'c6 ->
  x160:'d6 ->
  x161:'e6 ->
  x162:'f6 ->
  x163:'g6 ->
  x164:'h6 ->
  x165:'i6 ->
  x166:'j6 ->
  x167:'k6 ->
  x168:'l6 ->
  x169:'m6 ->
  x170:'n6 ->
  x171:'o6 ->
  x172:'p6 ->
  x173:'q6 ->
  x174:'r6 ->
  x175:'s6 ->
  x176:'t6 ->
  x177:'u6 ->
  x178:'v6 ->
  x179:'w6 ->
  x180:'x6 ->
  x181:'y6 ->
  x182:'z6 ->
  x183:'a7 ->
  x184:'b7 ->
  x185:'c7 ->
  x186:'d7 ->
  x187:'e7 ->
  x188:'f7 ->
  x189:'g7 ->
  x190:'h7 ->
  x191:'i7 ->
  x192:'j7 ->
  x193:'k7 ->
  x194:'l7 ->
  x195:'m7 ->
  x196:'n7 ->
  x197:'o7 ->
  x198:'p7 ->
  x199:'q7 ->
  x200:'r7 ->
  x201:'s7 ->
  x202:'t7 ->
  x203:'u7 ->
  x204:'v7 ->
  x205:'w7 ->
  x206:'x7 ->
  x207:'y7 ->
  x208:'z7 ->
  x209:'a8 ->
  x210:'b8 ->
  x211:'c8 ->
  x212:'d8 ->
  x213:'e8 ->
  x214:'f8 ->
  x215:'g8 ->
  x216:'h8 ->
  x217:'i8 ->
  x218:'j8 ->
  x219:'k8 ->
  x220:'l8 ->
  x221:'m8 ->
  x222:'n8 ->
  x223:'o8 ->
  x224:'p8 ->
  x225:'q8 ->
  x226:'r8 ->
  x227:'s8 ->
  x228:'t8 ->
  x229:'u8 ->
  x230:'v8 ->
  x231:'w8 ->
  x232:'x8 ->
  x233:'y8 ->
  x234:'z8 ->
  x235:'a9 ->
  x236:'b9 ->
  x237:'c9 ->
  x238:'d9 ->
  x239:'e9 ->
  x240:'f9 ->
  x241:'g9 ->
  x242:'h9 ->
  x243:'i9 ->
  x244:'j9 ->
  x245:'k9 ->
  x246:'l9 ->
  x247:'m9 ->
  x248:'n9 ->
  x249:'o9 ->
  x250:'p9 ->
  x251:'q9 ->
  x252:'r9 ->
  x253:'s9 ->
  x254:'t9 -> x255:'u9 -> x256:'v9 -> x257:'w9 -> x258:'x9 -> unit = <fun>
|}]


(* two split points, local returning *)
let two_splits__local_returning
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
  ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
  ~x149 ~x150 ~x151 ~x152 ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
  ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
  ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
  ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
  ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
  ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
  ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
  ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
  ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
  ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
    local_ (x1, x258)
;;

[%%expect {|
(let
  (two_splits__local_returning =
     (function {nlocal = 1} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138 x139 x140 x141
       x142 x143 x144 x145 x146 x147 x148 x149 x150 x151 x152 x153 x154 x155
       x156 x157 x158 x159 x160 x161 x162 x163 x164 x165 x166 x167 x168 x169
       x170 x171 x172 x173 x174 x175 x176 x177 x178 x179 x180 x181 x182 x183
       x184 x185 x186 x187 x188 x189 x190 x191 x192 x193 x194 x195 x196 x197
       x198 x199 x200 x201 x202 x203 x204 x205 x206 x207 x208 x209 x210 x211
       x212 x213 x214 x215 x216 x217 x218 x219 x220 x221 x222 x223 x224 x225
       x226 x227 x228 x229 x230 x231 x232 x233 x234 x235 x236 x237 x238 x239
       x240 x241 x242 x243 x244 x245 x246 x247 x248 x249 x250 x251 x252 x253
       x254 x255 x256 x257 x258
       [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x258)))
  (apply (field_imm 1 (global Toploop!)) "two_splits__local_returning"
    two_splits__local_returning))
val two_splits__local_returning :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 ->
  x128:'x4 ->
  x129:'y4 ->
  x130:'z4 ->
  x131:'a5 ->
  x132:'b5 ->
  x133:'c5 ->
  x134:'d5 ->
  x135:'e5 ->
  x136:'f5 ->
  x137:'g5 ->
  x138:'h5 ->
  x139:'i5 ->
  x140:'j5 ->
  x141:'k5 ->
  x142:'l5 ->
  x143:'m5 ->
  x144:'n5 ->
  x145:'o5 ->
  x146:'p5 ->
  x147:'q5 ->
  x148:'r5 ->
  x149:'s5 ->
  x150:'t5 ->
  x151:'u5 ->
  x152:'v5 ->
  x153:'w5 ->
  x154:'x5 ->
  x155:'y5 ->
  x156:'z5 ->
  x157:'a6 ->
  x158:'b6 ->
  x159:'c6 ->
  x160:'d6 ->
  x161:'e6 ->
  x162:'f6 ->
  x163:'g6 ->
  x164:'h6 ->
  x165:'i6 ->
  x166:'j6 ->
  x167:'k6 ->
  x168:'l6 ->
  x169:'m6 ->
  x170:'n6 ->
  x171:'o6 ->
  x172:'p6 ->
  x173:'q6 ->
  x174:'r6 ->
  x175:'s6 ->
  x176:'t6 ->
  x177:'u6 ->
  x178:'v6 ->
  x179:'w6 ->
  x180:'x6 ->
  x181:'y6 ->
  x182:'z6 ->
  x183:'a7 ->
  x184:'b7 ->
  x185:'c7 ->
  x186:'d7 ->
  x187:'e7 ->
  x188:'f7 ->
  x189:'g7 ->
  x190:'h7 ->
  x191:'i7 ->
  x192:'j7 ->
  x193:'k7 ->
  x194:'l7 ->
  x195:'m7 ->
  x196:'n7 ->
  x197:'o7 ->
  x198:'p7 ->
  x199:'q7 ->
  x200:'r7 ->
  x201:'s7 ->
  x202:'t7 ->
  x203:'u7 ->
  x204:'v7 ->
  x205:'w7 ->
  x206:'x7 ->
  x207:'y7 ->
  x208:'z7 ->
  x209:'a8 ->
  x210:'b8 ->
  x211:'c8 ->
  x212:'d8 ->
  x213:'e8 ->
  x214:'f8 ->
  x215:'g8 ->
  x216:'h8 ->
  x217:'i8 ->
  x218:'j8 ->
  x219:'k8 ->
  x220:'l8 ->
  x221:'m8 ->
  x222:'n8 ->
  x223:'o8 ->
  x224:'p8 ->
  x225:'q8 ->
  x226:'r8 ->
  x227:'s8 ->
  x228:'t8 ->
  x229:'u8 ->
  x230:'v8 ->
  x231:'w8 ->
  x232:'x8 ->
  x233:'y8 ->
  x234:'z8 ->
  x235:'a9 ->
  x236:'b9 ->
  x237:'c9 ->
  x238:'d9 ->
  x239:'e9 ->
  x240:'f9 ->
  x241:'g9 ->
  x242:'h9 ->
  x243:'i9 ->
  x244:'j9 ->
  x245:'k9 ->
  x246:'l9 ->
  x247:'m9 ->
  x248:'n9 ->
  x249:'o9 ->
  x250:'p9 ->
  x251:'q9 ->
  x252:'r9 ->
  x253:'s9 ->
  x254:'t9 -> x255:'u9 -> x256:'v9 -> x257:'w9 -> x258:'x9 -> local_ 'a * 'x9 =
  <fun>
|}]

(* two split points with a local argument *)
let two_splits_local_param
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
  ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
  ~x149 ~x150 ~x151 ~(local_ x152) ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
  ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
  ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
  ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
  ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
  ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
  ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
  ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
  ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
  ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
    ()
;;

[%%expect {|
(let
  (two_splits_local_param =
     (function {nlocal = 107} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138 x139 x140 x141
       x142 x143 x144 x145 x146 x147 x148 x149 x150 x151 x152[L] x153 x154
       x155 x156 x157 x158 x159 x160 x161 x162 x163 x164 x165 x166 x167 x168
       x169 x170 x171 x172 x173 x174 x175 x176 x177 x178 x179 x180 x181 x182
       x183 x184 x185 x186 x187 x188 x189 x190 x191 x192 x193 x194 x195 x196
       x197 x198 x199 x200 x201 x202 x203 x204 x205 x206 x207 x208 x209 x210
       x211 x212 x213 x214 x215 x216 x217 x218 x219 x220 x221 x222 x223 x224
       x225 x226 x227 x228 x229 x230 x231 x232 x233 x234 x235 x236 x237 x238
       x239 x240 x241 x242 x243 x244 x245 x246 x247 x248 x249 x250 x251 x252
       x253 x254 x255 x256 x257 x258 : int 0))
  (apply (field_imm 1 (global Toploop!)) "two_splits_local_param"
    two_splits_local_param))
val two_splits_local_param :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 ->
  x128:'x4 ->
  x129:'y4 ->
  x130:'z4 ->
  x131:'a5 ->
  x132:'b5 ->
  x133:'c5 ->
  x134:'d5 ->
  x135:'e5 ->
  x136:'f5 ->
  x137:'g5 ->
  x138:'h5 ->
  x139:'i5 ->
  x140:'j5 ->
  x141:'k5 ->
  x142:'l5 ->
  x143:'m5 ->
  x144:'n5 ->
  x145:'o5 ->
  x146:'p5 ->
  x147:'q5 ->
  x148:'r5 ->
  x149:'s5 ->
  x150:'t5 ->
  x151:'u5 ->
  x152:local_ 'v5 ->
  x153:'w5 ->
  x154:'x5 ->
  x155:'y5 ->
  x156:'z5 ->
  x157:'a6 ->
  x158:'b6 ->
  x159:'c6 ->
  x160:'d6 ->
  x161:'e6 ->
  x162:'f6 ->
  x163:'g6 ->
  x164:'h6 ->
  x165:'i6 ->
  x166:'j6 ->
  x167:'k6 ->
  x168:'l6 ->
  x169:'m6 ->
  x170:'n6 ->
  x171:'o6 ->
  x172:'p6 ->
  x173:'q6 ->
  x174:'r6 ->
  x175:'s6 ->
  x176:'t6 ->
  x177:'u6 ->
  x178:'v6 ->
  x179:'w6 ->
  x180:'x6 ->
  x181:'y6 ->
  x182:'z6 ->
  x183:'a7 ->
  x184:'b7 ->
  x185:'c7 ->
  x186:'d7 ->
  x187:'e7 ->
  x188:'f7 ->
  x189:'g7 ->
  x190:'h7 ->
  x191:'i7 ->
  x192:'j7 ->
  x193:'k7 ->
  x194:'l7 ->
  x195:'m7 ->
  x196:'n7 ->
  x197:'o7 ->
  x198:'p7 ->
  x199:'q7 ->
  x200:'r7 ->
  x201:'s7 ->
  x202:'t7 ->
  x203:'u7 ->
  x204:'v7 ->
  x205:'w7 ->
  x206:'x7 ->
  x207:'y7 ->
  x208:'z7 ->
  x209:'a8 ->
  x210:'b8 ->
  x211:'c8 ->
  x212:'d8 ->
  x213:'e8 ->
  x214:'f8 ->
  x215:'g8 ->
  x216:'h8 ->
  x217:'i8 ->
  x218:'j8 ->
  x219:'k8 ->
  x220:'l8 ->
  x221:'m8 ->
  x222:'n8 ->
  x223:'o8 ->
  x224:'p8 ->
  x225:'q8 ->
  x226:'r8 ->
  x227:'s8 ->
  x228:'t8 ->
  x229:'u8 ->
  x230:'v8 ->
  x231:'w8 ->
  x232:'x8 ->
  x233:'y8 ->
  x234:'z8 ->
  x235:'a9 ->
  x236:'b9 ->
  x237:'c9 ->
  x238:'d9 ->
  x239:'e9 ->
  x240:'f9 ->
  x241:'g9 ->
  x242:'h9 ->
  x243:'i9 ->
  x244:'j9 ->
  x245:'k9 ->
  x246:'l9 ->
  x247:'m9 ->
  x248:'n9 ->
  x249:'o9 ->
  x250:'p9 ->
  x251:'q9 ->
  x252:'r9 ->
  x253:'s9 ->
  x254:'t9 -> x255:'u9 -> x256:'v9 -> x257:'w9 -> x258:'x9 -> unit = <fun>
|}]

(* two split points with a local argument and local returning *)
let two_splits_local_param__local_returning
  ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
  ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
  ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
  ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
  ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
  ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
  ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
  ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
  ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
  ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
  ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
  ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
  ~x149 ~x150 ~x151 ~(local_ x152) ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
  ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
  ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
  ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
  ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
  ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
  ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
  ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
  ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
  ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
    local_ (x1, x152)
;;

[%%expect {|
(let
  (two_splits_local_param__local_returning =
     (function {nlocal = 107} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
       x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
       x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48
       x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64 x65
       x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81 x82
       x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98 x99
       x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112 x113
       x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125 x126 x127
       x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138 x139 x140 x141
       x142 x143 x144 x145 x146 x147 x148 x149 x150 x151 x152[L] x153 x154
       x155 x156 x157 x158 x159 x160 x161 x162 x163 x164 x165 x166 x167 x168
       x169 x170 x171 x172 x173 x174 x175 x176 x177 x178 x179 x180 x181 x182
       x183 x184 x185 x186 x187 x188 x189 x190 x191 x192 x193 x194 x195 x196
       x197 x198 x199 x200 x201 x202 x203 x204 x205 x206 x207 x208 x209 x210
       x211 x212 x213 x214 x215 x216 x217 x218 x219 x220 x221 x222 x223 x224
       x225 x226 x227 x228 x229 x230 x231 x232 x233 x234 x235 x236 x237 x238
       x239 x240 x241 x242 x243 x244 x245 x246 x247 x248 x249 x250 x251 x252
       x253 x254 x255 x256 x257 x258
       [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x152)))
  (apply (field_imm 1 (global Toploop!))
    "two_splits_local_param__local_returning"
    two_splits_local_param__local_returning))
val two_splits_local_param__local_returning :
  x1:'a ->
  x2:'b ->
  x3:'c ->
  x4:'d ->
  x5:'e ->
  x6:'f ->
  x7:'g ->
  x8:'h ->
  x9:'i ->
  x10:'j ->
  x11:'k ->
  x12:'l ->
  x13:'m ->
  x14:'n ->
  x15:'o ->
  x16:'p ->
  x17:'q ->
  x18:'r ->
  x19:'s ->
  x20:'t ->
  x21:'u ->
  x22:'v ->
  x23:'w ->
  x24:'x ->
  x25:'y ->
  x26:'z ->
  x27:'a1 ->
  x28:'b1 ->
  x29:'c1 ->
  x30:'d1 ->
  x31:'e1 ->
  x32:'f1 ->
  x33:'g1 ->
  x34:'h1 ->
  x35:'i1 ->
  x36:'j1 ->
  x37:'k1 ->
  x38:'l1 ->
  x39:'m1 ->
  x40:'n1 ->
  x41:'o1 ->
  x42:'p1 ->
  x43:'q1 ->
  x44:'r1 ->
  x45:'s1 ->
  x46:'t1 ->
  x47:'u1 ->
  x48:'v1 ->
  x49:'w1 ->
  x50:'x1 ->
  x51:'y1 ->
  x52:'z1 ->
  x53:'a2 ->
  x54:'b2 ->
  x55:'c2 ->
  x56:'d2 ->
  x57:'e2 ->
  x58:'f2 ->
  x59:'g2 ->
  x60:'h2 ->
  x61:'i2 ->
  x62:'j2 ->
  x63:'k2 ->
  x64:'l2 ->
  x65:'m2 ->
  x66:'n2 ->
  x67:'o2 ->
  x68:'p2 ->
  x69:'q2 ->
  x70:'r2 ->
  x71:'s2 ->
  x72:'t2 ->
  x73:'u2 ->
  x74:'v2 ->
  x75:'w2 ->
  x76:'x2 ->
  x77:'y2 ->
  x78:'z2 ->
  x79:'a3 ->
  x80:'b3 ->
  x81:'c3 ->
  x82:'d3 ->
  x83:'e3 ->
  x84:'f3 ->
  x85:'g3 ->
  x86:'h3 ->
  x87:'i3 ->
  x88:'j3 ->
  x89:'k3 ->
  x90:'l3 ->
  x91:'m3 ->
  x92:'n3 ->
  x93:'o3 ->
  x94:'p3 ->
  x95:'q3 ->
  x96:'r3 ->
  x97:'s3 ->
  x98:'t3 ->
  x99:'u3 ->
  x100:'v3 ->
  x101:'w3 ->
  x102:'x3 ->
  x103:'y3 ->
  x104:'z3 ->
  x105:'a4 ->
  x106:'b4 ->
  x107:'c4 ->
  x108:'d4 ->
  x109:'e4 ->
  x110:'f4 ->
  x111:'g4 ->
  x112:'h4 ->
  x113:'i4 ->
  x114:'j4 ->
  x115:'k4 ->
  x116:'l4 ->
  x117:'m4 ->
  x118:'n4 ->
  x119:'o4 ->
  x120:'p4 ->
  x121:'q4 ->
  x122:'r4 ->
  x123:'s4 ->
  x124:'t4 ->
  x125:'u4 ->
  x126:'v4 ->
  x127:'w4 ->
  x128:'x4 ->
  x129:'y4 ->
  x130:'z4 ->
  x131:'a5 ->
  x132:'b5 ->
  x133:'c5 ->
  x134:'d5 ->
  x135:'e5 ->
  x136:'f5 ->
  x137:'g5 ->
  x138:'h5 ->
  x139:'i5 ->
  x140:'j5 ->
  x141:'k5 ->
  x142:'l5 ->
  x143:'m5 ->
  x144:'n5 ->
  x145:'o5 ->
  x146:'p5 ->
  x147:'q5 ->
  x148:'r5 ->
  x149:'s5 ->
  x150:'t5 ->
  x151:'u5 ->
  x152:local_ 'v5 ->
  x153:'w5 ->
  x154:'x5 ->
  x155:'y5 ->
  x156:'z5 ->
  x157:'a6 ->
  x158:'b6 ->
  x159:'c6 ->
  x160:'d6 ->
  x161:'e6 ->
  x162:'f6 ->
  x163:'g6 ->
  x164:'h6 ->
  x165:'i6 ->
  x166:'j6 ->
  x167:'k6 ->
  x168:'l6 ->
  x169:'m6 ->
  x170:'n6 ->
  x171:'o6 ->
  x172:'p6 ->
  x173:'q6 ->
  x174:'r6 ->
  x175:'s6 ->
  x176:'t6 ->
  x177:'u6 ->
  x178:'v6 ->
  x179:'w6 ->
  x180:'x6 ->
  x181:'y6 ->
  x182:'z6 ->
  x183:'a7 ->
  x184:'b7 ->
  x185:'c7 ->
  x186:'d7 ->
  x187:'e7 ->
  x188:'f7 ->
  x189:'g7 ->
  x190:'h7 ->
  x191:'i7 ->
  x192:'j7 ->
  x193:'k7 ->
  x194:'l7 ->
  x195:'m7 ->
  x196:'n7 ->
  x197:'o7 ->
  x198:'p7 ->
  x199:'q7 ->
  x200:'r7 ->
  x201:'s7 ->
  x202:'t7 ->
  x203:'u7 ->
  x204:'v7 ->
  x205:'w7 ->
  x206:'x7 ->
  x207:'y7 ->
  x208:'z7 ->
  x209:'a8 ->
  x210:'b8 ->
  x211:'c8 ->
  x212:'d8 ->
  x213:'e8 ->
  x214:'f8 ->
  x215:'g8 ->
  x216:'h8 ->
  x217:'i8 ->
  x218:'j8 ->
  x219:'k8 ->
  x220:'l8 ->
  x221:'m8 ->
  x222:'n8 ->
  x223:'o8 ->
  x224:'p8 ->
  x225:'q8 ->
  x226:'r8 ->
  x227:'s8 ->
  x228:'t8 ->
  x229:'u8 ->
  x230:'v8 ->
  x231:'w8 ->
  x232:'x8 ->
  x233:'y8 ->
  x234:'z8 ->
  x235:'a9 ->
  x236:'b9 ->
  x237:'c9 ->
  x238:'d9 ->
  x239:'e9 ->
  x240:'f9 ->
  x241:'g9 ->
  x242:'h9 ->
  x243:'i9 ->
  x244:'j9 ->
  x245:'k9 ->
  x246:'l9 ->
  x247:'m9 ->
  x248:'n9 ->
  x249:'o9 ->
  x250:'p9 ->
  x251:'q9 ->
  x252:'r9 ->
  x253:'s9 ->
  x254:'t9 -> x255:'u9 -> x256:'v9 -> x257:'w9 -> x258:'x9 -> local_ 'a * 'v5 =
  <fun>
|}]

(* The functions themselves are locally allocated *)
let _ =
  (* No local arguments *)
  let local_ no_local_params
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 = ()
  in
  ()
;;

[%%expect {|
(region
  (let
    (no_local_params =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130 x131 : int 0))
    0))
- : unit = ()
|}]

let _ =
  (* No local arguments; local returning *)
  let local_ no_local_params__local_returning
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x2)
  in
  ()
;;

[%%expect {|
(region
  (let
    (no_local_params__local_returning =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130 x131
         [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x2)))
    0))
- : unit = ()
|}]

let _ =
  (* first local argument comes after the split point *)
  let local_ local_param_after_split
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~(local_ x130) ~x131 = ()
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_after_split =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130[L] x131 : int 0))
    0))
- : unit = ()
|}]


let _ =
  (* first local argument comes after the split point; local returning *)
  let local_ local_param_after_split__local_returning
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~(local_ x130) ~x131 = local_ (x1, x130)
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_after_split__local_returning =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130[L] x131
         [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x130)))
    0))
- : unit = ()
|}]

let _ =
  (* first local argument comes immediately after the split point *)
  let local_ local_param_just_after_split
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~(local_ x127) ~x128 ~x129 ~x130 ~x131 = ()
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_just_after_split =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127[L] x128 x129 x130 x131 : int 0))
    0))
- : unit = ()
|}]

let _ =
  (* first local argument comes immediately after the split point;
     local returning
   *)
  let local_ local_param_just_after_split__local_returning
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~(local_ x127) ~x128 ~x129 ~x130 ~x131 = local_ (x1, x127)
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_just_after_split__local_returning =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127[L] x128 x129 x130 x131
         [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x127)))
    0))
- : unit = ()
|}]

let _ =
  (* first local argument comes immediately before the split point *)
  let local_ local_param_just_before_split
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~(local_ x126)
    ~x127 ~x128 ~x129 ~x130 ~x131 = ()
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_just_before_split =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126[L] x127 x128 x129 x130 x131 : int 0))
    0))
- : unit = ()
|}]

let _ =
  (* first local argument comes immediately before the split point;
     local returning
   *)
  let local_ local_param_just_before_split__local_returning
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~(local_ x126)
    ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x126)
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_just_before_split__local_returning =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126[L] x127 x128 x129 x130 x131
         [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x126)))
    0))
- : unit = ()
|}]

let _ =
  (* first local argument comes well before the split point *)
  let local_ local_param_before_split
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~(local_ x8) ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 = ()
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_before_split =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8[L] x9 x10 x11 x12
         x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29
         x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46
         x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
         x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80
         x81 x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97
         x98 x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111
         x112 x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124
         x125 x126 x127 x128 x129 x130 x131 : int 0))
    0))
- : unit = ()
|}]

let _ =
  (* first local argument comes well before the split point; local returning *)
  let local_ local_param_before_split__local_returning
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~(local_ x8) ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x8)
  in
  ()
;;

[%%expect {|
(region
  (let
    (local_param_before_split__local_returning =
       (function[L] {nlocal = 131} x1 x2 x3 x4 x5 x6 x7 x8[L] x9 x10 x11 x12
         x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29
         x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46
         x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
         x64 x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80
         x81 x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97
         x98 x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111
         x112 x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124
         x125 x126 x127 x128 x129 x130 x131
         [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x8)))
    0))
- : unit = ()
|}]

let _ =
  (* two split points *)
  let local_ two_splits
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
    ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
    ~x149 ~x150 ~x151 ~x152 ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
    ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
    ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
    ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
    ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
    ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
    ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
    ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
    ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
    ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
      ()
  in
  ()
;;

[%%expect {|
(region
  (let
    (two_splits =
       (function[L] {nlocal = 258} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138
         x139 x140 x141 x142 x143 x144 x145 x146 x147 x148 x149 x150 x151
         x152 x153 x154 x155 x156 x157 x158 x159 x160 x161 x162 x163 x164
         x165 x166 x167 x168 x169 x170 x171 x172 x173 x174 x175 x176 x177
         x178 x179 x180 x181 x182 x183 x184 x185 x186 x187 x188 x189 x190
         x191 x192 x193 x194 x195 x196 x197 x198 x199 x200 x201 x202 x203
         x204 x205 x206 x207 x208 x209 x210 x211 x212 x213 x214 x215 x216
         x217 x218 x219 x220 x221 x222 x223 x224 x225 x226 x227 x228 x229
         x230 x231 x232 x233 x234 x235 x236 x237 x238 x239 x240 x241 x242
         x243 x244 x245 x246 x247 x248 x249 x250 x251 x252 x253 x254 x255
         x256 x257 x258 : int 0))
    0))
- : unit = ()
|}]

let _ =
  (* two split points, local returning *)
  let local_ two_splits__local_returning
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
    ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
    ~x149 ~x150 ~x151 ~x152 ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
    ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
    ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
    ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
    ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
    ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
    ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
    ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
    ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
    ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
      local_ (x1, x258)
  in
  ()
;;

[%%expect {|
(region
  (let
    (two_splits__local_returning =
       (function[L] {nlocal = 258} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138
         x139 x140 x141 x142 x143 x144 x145 x146 x147 x148 x149 x150 x151
         x152 x153 x154 x155 x156 x157 x158 x159 x160 x161 x162 x163 x164
         x165 x166 x167 x168 x169 x170 x171 x172 x173 x174 x175 x176 x177
         x178 x179 x180 x181 x182 x183 x184 x185 x186 x187 x188 x189 x190
         x191 x192 x193 x194 x195 x196 x197 x198 x199 x200 x201 x202 x203
         x204 x205 x206 x207 x208 x209 x210 x211 x212 x213 x214 x215 x216
         x217 x218 x219 x220 x221 x222 x223 x224 x225 x226 x227 x228 x229
         x230 x231 x232 x233 x234 x235 x236 x237 x238 x239 x240 x241 x242
         x243 x244 x245 x246 x247 x248 x249 x250 x251 x252 x253 x254 x255
         x256 x257 x258
         [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x258)))
    0))
- : unit = ()
|}]

let _ =
  (* two split points with a local argument *)
  let local_ two_splits_local_param
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
    ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
    ~x149 ~x150 ~x151 ~(local_ x152) ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
    ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
    ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
    ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
    ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
    ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
    ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
    ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
    ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
    ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
      ()
  in
  ()
;;

[%%expect {|
(region
  (let
    (two_splits_local_param =
       (function[L] {nlocal = 258} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138
         x139 x140 x141 x142 x143 x144 x145 x146 x147 x148 x149 x150 x151
         x152[L] x153 x154 x155 x156 x157 x158 x159 x160 x161 x162 x163 x164
         x165 x166 x167 x168 x169 x170 x171 x172 x173 x174 x175 x176 x177
         x178 x179 x180 x181 x182 x183 x184 x185 x186 x187 x188 x189 x190
         x191 x192 x193 x194 x195 x196 x197 x198 x199 x200 x201 x202 x203
         x204 x205 x206 x207 x208 x209 x210 x211 x212 x213 x214 x215 x216
         x217 x218 x219 x220 x221 x222 x223 x224 x225 x226 x227 x228 x229
         x230 x231 x232 x233 x234 x235 x236 x237 x238 x239 x240 x241 x242
         x243 x244 x245 x246 x247 x248 x249 x250 x251 x252 x253 x254 x255
         x256 x257 x258 : int 0))
    0))
- : unit = ()
|}]

let _ =
  (* two split points with a local argument and local returning *)
  let local_ two_splits_local_param__local_returning
    ~x1 ~x2 ~x3 ~x4 ~x5 ~x6 ~x7 ~x8 ~x9 ~x10 ~x11 ~x12 ~x13 ~x14
    ~x15 ~x16 ~x17 ~x18 ~x19 ~x20 ~x21 ~x22 ~x23 ~x24 ~x25 ~x26 ~x27
    ~x28 ~x29 ~x30 ~x31 ~x32 ~x33 ~x34 ~x35 ~x36 ~x37 ~x38 ~x39 ~x40
    ~x41 ~x42 ~x43 ~x44 ~x45 ~x46 ~x47 ~x48 ~x49 ~x50 ~x51 ~x52 ~x53
    ~x54 ~x55 ~x56 ~x57 ~x58 ~x59 ~x60 ~x61 ~x62 ~x63 ~x64 ~x65 ~x66
    ~x67 ~x68 ~x69 ~x70 ~x71 ~x72 ~x73 ~x74 ~x75 ~x76 ~x77 ~x78 ~x79
    ~x80 ~x81 ~x82 ~x83 ~x84 ~x85 ~x86 ~x87 ~x88 ~x89 ~x90 ~x91 ~x92
    ~x93 ~x94 ~x95 ~x96 ~x97 ~x98 ~x99 ~x100 ~x101 ~x102 ~x103 ~x104
    ~x105 ~x106 ~x107 ~x108 ~x109 ~x110 ~x111 ~x112 ~x113 ~x114 ~x115
    ~x116 ~x117 ~x118 ~x119 ~x120 ~x121 ~x122 ~x123 ~x124 ~x125 ~x126
    ~x127 ~x128 ~x129 ~x130 ~x131 ~x132 ~x133 ~x134 ~x135 ~x136 ~x137
    ~x138 ~x139 ~x140 ~x141 ~x142 ~x143 ~x144 ~x145 ~x146 ~x147 ~x148
    ~x149 ~x150 ~x151 ~(local_ x152) ~x153 ~x154 ~x155 ~x156 ~x157 ~x158 ~x159
    ~x160 ~x161 ~x162 ~x163 ~x164 ~x165 ~x166 ~x167 ~x168 ~x169 ~x170
    ~x171 ~x172 ~x173 ~x174 ~x175 ~x176 ~x177 ~x178 ~x179 ~x180 ~x181
    ~x182 ~x183 ~x184 ~x185 ~x186 ~x187 ~x188 ~x189 ~x190 ~x191 ~x192
    ~x193 ~x194 ~x195 ~x196 ~x197 ~x198 ~x199 ~x200 ~x201 ~x202 ~x203
    ~x204 ~x205 ~x206 ~x207 ~x208 ~x209 ~x210 ~x211 ~x212 ~x213 ~x214
    ~x215 ~x216 ~x217 ~x218 ~x219 ~x220 ~x221 ~x222 ~x223 ~x224 ~x225
    ~x226 ~x227 ~x228 ~x229 ~x230 ~x231 ~x232 ~x233 ~x234 ~x235 ~x236
    ~x237 ~x238 ~x239 ~x240 ~x241 ~x242 ~x243 ~x244 ~x245 ~x246 ~x247
    ~x248 ~x249 ~x250 ~x251 ~x252 ~x253 ~x254 ~x255 ~x256 ~x257 ~x258 =
      local_ (x1, x152)
  in
  ()
;;

[%%expect {|
(region
  (let
    (two_splits_local_param__local_returning =
       (function[L] {nlocal = 258} x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
         x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30
         x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47
         x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63 x64
         x65 x66 x67 x68 x69 x70 x71 x72 x73 x74 x75 x76 x77 x78 x79 x80 x81
         x82 x83 x84 x85 x86 x87 x88 x89 x90 x91 x92 x93 x94 x95 x96 x97 x98
         x99 x100 x101 x102 x103 x104 x105 x106 x107 x108 x109 x110 x111 x112
         x113 x114 x115 x116 x117 x118 x119 x120 x121 x122 x123 x124 x125
         x126 x127 x128 x129 x130 x131 x132 x133 x134 x135 x136 x137 x138
         x139 x140 x141 x142 x143 x144 x145 x146 x147 x148 x149 x150 x151
         x152[L] x153 x154 x155 x156 x157 x158 x159 x160 x161 x162 x163 x164
         x165 x166 x167 x168 x169 x170 x171 x172 x173 x174 x175 x176 x177
         x178 x179 x180 x181 x182 x183 x184 x185 x186 x187 x188 x189 x190
         x191 x192 x193 x194 x195 x196 x197 x198 x199 x200 x201 x202 x203
         x204 x205 x206 x207 x208 x209 x210 x211 x212 x213 x214 x215 x216
         x217 x218 x219 x220 x221 x222 x223 x224 x225 x226 x227 x228 x229
         x230 x231 x232 x233 x234 x235 x236 x237 x238 x239 x240 x241 x242
         x243 x244 x245 x246 x247 x248 x249 x250 x251 x252 x253 x254 x255
         x256 x257 x258
         [(consts ()) (non_consts ([0: *, *]))](makelocalblock 0 x1 x152)))
    0))
- : unit = ()
|}]
