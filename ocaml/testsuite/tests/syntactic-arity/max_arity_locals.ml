(* TEST

 flags = "-dlambda"
 * setup-ocamlopt.byte-build-env
 ** ocamlopt.byte
 *** check-ocamlopt.byte-output
 **** run
 ***** check-program-output
*)

(* Observe a case where a function's arity is a different notion
   than native code arity (i.e. the number of arguments required
   to enter the "fast path" where arguments are passed in registers/in
   the argument buffer).

   The max native code arity is 128, but the side-effects here don't run
   until after all 133 arguments are provided.
 *)

(* No local arguments *)
let f1
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
  ~x127 ~x128 ~x129 ~x130 ~x131 = ();;

(* first local argument comes after the split point *)
let f2
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
  ~x127 ~x128 ~x129 ~(local_ x130) ~x131 = ();;

(* first local argument comes immediately after the split point *)
let f3
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
  ~(local_ x127) ~x128 ~x129 ~x130 ~x131 = ();;

(* first local argument comes immediately before the split point *)
let f4
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
  ~x127 ~x128 ~x129 ~x130 ~x131 = ();;

(* first local argument comes well before the split point *)
let f5
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
  ~x127 ~x128 ~x129 ~x130 ~x131 = ();;
