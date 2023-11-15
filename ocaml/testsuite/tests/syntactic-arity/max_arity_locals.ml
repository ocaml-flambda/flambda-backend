(* TEST

 flags = "-dlambda -dno-unique-ids -w +unused-value-declaration"

* flambda
** stack-allocation
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
***** check-ocamlopt.byte-output
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
  ~x127 ~x128 ~x129 ~x130 ~x131 = ();;

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
  ~x127 ~x128 ~x129 ~(local_ x130) ~x131 = ();;

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
  ~(local_ x127) ~x128 ~x129 ~x130 ~x131 = ();;

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
  ~(local_ x127) ~x128 ~x129 ~x130 ~x131 = local_ (x1, x127);;

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
  ~x127 ~x128 ~x129 ~x130 ~x131 = ();;

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
  ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x126);;

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
  ~x127 ~x128 ~x129 ~x130 ~x131 = ();;

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
  ~x127 ~x128 ~x129 ~x130 ~x131 = local_ (x1, x8);;

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

(* The functions themselves are locally allocated *)
let create_local () = local_

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
  ( no_local_params
  , no_local_params__local_returning
  , local_param_after_split
  , local_param_after_split__local_returning
  , local_param_just_after_split
  , local_param_just_after_split__local_returning
  , local_param_just_before_split
  , local_param_just_before_split__local_returning
  , local_param_before_split
  , local_param_before_split__local_returning
  , two_splits
  , two_splits__local_returning
  , two_splits_local_param
  , two_splits_local_param__local_returning
  )
