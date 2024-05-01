(* TEST
 flambda2;
 {
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }
*)

(*****************************************)
(* Prelude: Functions on unboxed floats. *)

module Float_u = struct
  include Stdlib__Float_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = pow
  let ( > ) x y = (compare x y) > 0
end

module Int32_u = Stdlib__Int32_u
module Int64_u = Stdlib__Int64_u
module Nativeint_u = Stdlib__Nativeint_u

let print_floatu prefix x = Printf.printf "%s: %.2f\n" prefix (Float_u.to_float x)
let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x
let print_int prefix x = Printf.printf "%s: %d\n" prefix x

let dummy = "dummy"

(*************************************************)
(* Test 1: basic mixed records involving float# *)

type mixedargs = { x2_1 : float;
                   x4_1 : float;
                   x6_1 : float;
                   x8_1 : float;
                   (* We include the string field to document more plainly that
                      the preceding section of [float] fields are boxed.
                      Without the [str] field, an implementation of mixed blocks
                      could more conceivably unbox the [float] fields.
                   *)
                   dummy : string;
                   x0_1 : int;
                   x0_2 : int;
                   x1 : float#;
                   x2_2 : int;
                   x3 : float#;
                   x4_2 : int;
                   x5 : float#;
                   x6_2 : int;
                   x7 : float#;
                   x8_2 : int;
                   x9 : float# }

(* Get some float# args by pattern matching and others by projection *)
let[@inline_never] f1 steps ({ x1; x0_1=start_k; x0_2=end_k; x8_1; x8_2; x5;
                               x6_1; x6_2 } as fargs) () =
  let[@inline never] rec go k =
    if k = end_k
    then Float_u.of_float 0.
    else begin
      let (x2_1, x2_2) = (fargs.x2_1, fargs.x2_2) in
      let {x4_1; x4_2; _} = fargs in
      let sum =
        Float_u.(of_float x2_1 + of_int x2_2 + of_float x4_1 + of_int x4_2
                 + of_float x6_1 + of_int x6_2 + of_float x8_1 + of_int x8_2)
      in
      let acc = go (k + 1) in
      steps.(k) <- Float_u.to_float acc;
      Float_u.(acc + ((x1 + fargs.x3 + x5 + fargs.x7 + fargs.x9)
                      * sum))
    end
  in
  go start_k

let test1 () =
  (* same math as f3_manyargs *)
  let steps = Array.init 10 (fun _ -> 0.0) in
  let x1 = Float_u.of_float 3.14 in
  let x3 = Float_u.of_float 2.72 in
  let x5 = Float_u.of_float 1.62 in
  let x7 = Float_u.of_float 1.41 in
  let x9 = Float_u.of_float 42.0 in

  (* these sum to 3.0 *)
  let x2_1 = 6.6 in
  let x2_2 = 42 in
  let x4_1 = -22.9 in
  let x4_2 = 109 in
  let x6_1 = -241.2 in
  let x6_2 = 90 in
  let x8_1 = -2.5 in
  let x8_2 = 22 in

  let fargs =
    { x0_1 = 4; x0_2 = 8; x1; x2_1; x2_2; x3; x4_1; x4_2; x5; x6_1; x6_2; x7;
      x8_1; x8_2; x9;
      dummy }
  in

  let f1 = f1 steps fargs in
  print_floatu "Test 1, 610.68: " (f1 ());
  Array.iteri (Printf.printf "  Test 1, step %d: %.2f\n") steps

let _ = test1 ()

(*****************************************************)
(* Test 2: mixed record manipulation *)

type t2 = { a : float;
            dummy : string;
             mutable b : int;
             c : float#;
             mutable d : float#;
             e : int;
             mutable f : float# }

(* Construction *)
let t2_1 = { a = 3.14;
              b = 13;
              c = Float_u.of_float 7.31;
              d = Float_u.of_float 1.41;
              e = 6;
              f = Float_u.of_float 27.1;
              dummy;
            }

let t2_2 = { a = (-3.14);
              b = -13;
              c = Float_u.of_float (-7.31);
              d = Float_u.of_float (-1.41);
              e = -6;
              f = Float_u.of_float (-27.1);
              dummy;
            }

let print_t2 t2 =
  print_float "  a" t2.a;
  print_int "  b" t2.b;
  print_floatu "  c" t2.c;
  print_floatu "  d" t2.d;
  print_int "  e" t2.e;
  print_floatu "  f" t2.f

let _ =
  Printf.printf "Test 2, construction:\n";
  print_t2 t2_1;
  print_t2 t2_2

(* Matching, projection *)
let f2_1 {c; d; f; _} r =
  match r with
  | { a; _ } ->
    { a = Float.of_int r.e;
      b = Float_u.(to_int (of_float a - d));
      c = Float_u.(r.c + c);
      d = Float_u.(d - (of_int r.b));
      e = Float_u.(to_int (f + (of_int r.e)));
      f = r.f;
      dummy}

let _ =
  Printf.printf "Test 2, matching and projection:\n";
  print_t2 (f2_1 t2_1 t2_2)

(* Record update and mutation *)
let f2_2 ({a; d; _} as r1) r2 =
  r1.d <- Float_u.of_float 42.0;
  let r3 = { r2 with c = r1.d;
                     d = Float_u.of_float 25.0 }
  in
  r3.b <- Float_u.(to_int (of_float a + d));
  r2.b <- 17;
  r1.f <- r2.c;
  r3

let _ =
  Printf.printf "Test 2, record update and mutation:\n";
  let t2_3 = f2_2 t2_1 t2_2 in
  print_t2 t2_1;
  print_t2 t2_2;
  print_t2 t2_3

(************************************************************)
(* Test 3: (float# + immediate) records in recursive groups *)

let rec f r =
  r.d <- Float_u.of_int t3_1.b;
  t3_2.b <- 42;
  t3_1.f <- Float_u.of_float t3_1.a;
  Float_u.(of_float r.a + of_float t3_2.a)


and t3_1 = { a = 1.1;
              b = 2;
              c = Float_u.of_float 3.3;
              d = Float_u.of_float 4.4;
              e = 5;
              f = Float_u.of_float 6.6;
              dummy;
  }

and t3_2 = { a = (- 5.1);
              b = -6;
              c = Float_u.of_float (-7.3);
              d = Float_u.of_float (-8.4);
              e = -9;
              f = Float_u.of_float (-10.6);
              dummy;
            }

let _ =
  Printf.printf "Test 3, (float#+imm) records in recursive groups:\n";
  print_t2 t3_1;
  print_t2 t3_2;
  let result = f t3_1 in
  print_floatu "  result (-4.00)" result;
  print_t2 t3_1;
  print_t2 t3_2

(******************************)
(* Test 4: large mixed blocks *)

(* Allocations of large-enough blocks go through a different code path in the
   backend and runtime. This mixed block is larger than that threshold.
 *)

type uf = float#
type i32 = int32#
type i64 = int64#
type i_n = nativeint#
type t4 =
  { x : string;
    f1 : uf; f2 : i_n; f3 : uf; f4 : uf; f5 : uf; f6 : uf; f7 : uf;
    f8 : i32; f9 : i64; f10 : uf; f11 : uf; f12 : i_n; f13 : uf; f14 : uf;
    f15 : uf; f16 : uf; f17 : uf; f18 : i32; f19 : i64; f20 : uf; f21 : uf;
    f22 : i_n; f23 : uf; f24 : uf; f25 : uf; f26 : uf; f27 : uf; f28 : i32;
    f29 : i64; f30 : uf; f31 : uf; f32 : i_n; f33 : uf; f34 : uf; f35 : uf;
    f36 : uf; f37 : uf; f38 : i32; f39 : i64; f40 : uf; f41 : uf; f42 : i_n;
    f43 : uf; f44 : uf; f45 : uf; f46 : uf; f47 : uf; f48 : i32; f49 : i64;
    f50 : uf; f51 : uf; f52 : i_n; f53 : uf; f54 : uf; f55 : uf; f56 : uf;
    f57 : uf; f58 : i32; f59 : i64; f60 : uf; f61 : uf; f62 : i_n; f63 : uf;
    f64 : uf; f65 : uf; f66 : uf; f67 : uf; f68 : i32; f69 : i64; f70 : uf;
    f71 : uf; f72 : i_n; f73 : uf; f74 : uf; f75 : uf; f76 : uf; f77 : uf;
    f78 : i32; f79 : i64; f80 : uf; f81 : uf; f82 : i_n; f83 : uf; f84 : uf;
    f85 : uf; f86 : uf; f87 : uf; f88 : i32; f89 : i64; f90 : uf; f91 : uf;
    f92 : i_n; f93 : uf; f94 : uf; f95 : uf; f96 : uf; f97 : uf; f98 : i32;
    f99 : i64; f100 : uf; f101 : uf; f102 : i_n; f103 : uf; f104 : uf; f105 : uf;
    f106 : uf; f107 : uf; f108 : i32; f109 : i64; f110 : uf; f111 : uf; f112 : i_n;
    f113 : uf; f114 : uf; f115 : uf; f116 : uf; f117 : uf; f118 : i32; f119 : i64;
    f120 : uf; f121 : uf; f122 : i_n; f123 : uf; f124 : uf; f125 : uf; f126 : uf;
    f127 : uf; f128 : i32; f129 : i64; f130 : uf; f131 : uf; f132 : i_n; f133 : uf;
    f134 : uf; f135 : uf; f136 : uf; f137 : uf; f138 : i32; f139 : i64; f140 : uf;
    f141 : uf; f142 : i_n; f143 : uf; f144 : uf; f145 : uf; f146 : uf; f147 : uf;
    f148 : i32; f149 : i64; f150 : uf; f151 : uf; f152 : i_n; f153 : uf; f154 : uf;
    f155 : uf; f156 : uf; f157 : uf; f158 : i32; f159 : i64; f160 : uf; f161 : uf;
    f162 : i_n; f163 : uf; f164 : uf; f165 : uf; f166 : uf; f167 : uf; f168 : i32;
    f169 : i64; f170 : uf; f171 : uf; f172 : i_n; f173 : uf; f174 : uf; f175 : uf;
    f176 : uf; f177 : uf; f178 : i32; f179 : i64; f180 : uf; f181 : uf; f182 : i_n;
    f183 : uf; f184 : uf; f185 : uf; f186 : uf; f187 : uf; f188 : i32; f189 : i64;
    f190 : uf; f191 : uf; f192 : i_n; f193 : uf; f194 : uf; f195 : uf; f196 : uf;
    f197 : uf; f198 : i32; f199 : i64; f200 : uf; f201 : uf; f202 : i_n; f203 : uf;
    f204 : uf; f205 : uf; f206 : uf; f207 : uf; f208 : i32; f209 : i64; f210 : uf;
    f211 : uf; f212 : i_n; f213 : uf; f214 : uf; f215 : uf; f216 : uf; f217 : uf;
    f218 : i32; f219 : i64; f220 : uf; f221 : uf; f222 : i_n; f223 : uf; f224 : uf;
    f225 : uf; f226 : uf; f227 : uf; f228 : i32; f229 : i64; f230 : uf; f231 : uf;
    f232 : i_n; f233 : uf; f234 : uf; f235 : uf; f236 : uf; f237 : uf; f238 : i32;
    f239 : i64; f240 : uf; f241 : uf; f242 : i_n; f243 : uf; f244 : uf; f245 : uf;
    f246 : uf; f247 : uf; f248 : i32; f249 : i64; f250 : uf; f251 : uf; f252 : i_n;
    f253 : uf; f254 : uf; f255 : uf; f256 : uf; f257 : uf; f258 : i32; f259 : i64;
  }

let create_t4 f i32 i64 i_n =
  { x = "prefix";
  f1=f 1;f2=i_n 2;f3=f 3;f4=f 4;f5=f 5;f6=f 6;f7=f 7;
  f8=i32 8;f9=i64 9;f10=f 10;f11=f 11;f12=i_n 12;f13=f 13;f14=f 14;
  f15=f 15;f16=f 16;f17=f 17;f18=i32 18;f19=i64 19;f20=f 20;f21=f 21;
  f22=i_n 22;f23=f 23;f24=f 24;f25=f 25;f26=f 26;f27=f 27;f28=i32 28;
  f29=i64 29;f30=f 30;f31=f 31;f32=i_n 32;f33=f 33;f34=f 34;f35=f 35;
  f36=f 36;f37=f 37;f38=i32 38;f39=i64 39;f40=f 40;f41=f 41;f42=i_n 42;
  f43=f 43;f44=f 44;f45=f 45;f46=f 46;f47=f 47;f48=i32 48;f49=i64 49;
  f50=f 50;f51=f 51;f52=i_n 52;f53=f 53;f54=f 54;f55=f 55;f56=f 56;
  f57=f 57;f58=i32 58;f59=i64 59;f60=f 60;f61=f 61;f62=i_n 62;f63=f 63;
  f64=f 64;f65=f 65;f66=f 66;f67=f 67;f68=i32 68;f69=i64 69;f70=f 70;
  f71=f 71;f72=i_n 72;f73=f 73;f74=f 74;f75=f 75;f76=f 76;f77=f 77;
  f78=i32 78;f79=i64 79;f80=f 80;f81=f 81;f82=i_n 82;f83=f 83;f84=f 84;
  f85=f 85;f86=f 86;f87=f 87;f88=i32 88;f89=i64 89;f90=f 90;f91=f 91;
  f92=i_n 92;f93=f 93;f94=f 94;f95=f 95;f96=f 96;f97=f 97;f98=i32 98;
  f99=i64 99;f100=f 100;f101=f 101;f102=i_n 102;f103=f 103;f104=f 104;f105=f 105;
  f106=f 106;f107=f 107;f108=i32 108;f109=i64 109;f110=f 110;f111=f 111;f112=i_n 112;
  f113=f 113;f114=f 114;f115=f 115;f116=f 116;f117=f 117;f118=i32 118;f119=i64 119;
  f120=f 120;f121=f 121;f122=i_n 122;f123=f 123;f124=f 124;f125=f 125;f126=f 126;
  f127=f 127;f128=i32 128;f129=i64 129;f130=f 130;f131=f 131;f132=i_n 132;f133=f 133;
  f134=f 134;f135=f 135;f136=f 136;f137=f 137;f138=i32 138;f139=i64 139;f140=f 140;
  f141=f 141;f142=i_n 142;f143=f 143;f144=f 144;f145=f 145;f146=f 146;f147=f 147;
  f148=i32 148;f149=i64 149;f150=f 150;f151=f 151;f152=i_n 152;f153=f 153;f154=f 154;
  f155=f 155;f156=f 156;f157=f 157;f158=i32 158;f159=i64 159;f160=f 160;f161=f 161;
  f162=i_n 162;f163=f 163;f164=f 164;f165=f 165;f166=f 166;f167=f 167;f168=i32 168;
  f169=i64 169;f170=f 170;f171=f 171;f172=i_n 172;f173=f 173;f174=f 174;f175=f 175;
  f176=f 176;f177=f 177;f178=i32 178;f179=i64 179;f180=f 180;f181=f 181;f182=i_n 182;
  f183=f 183;f184=f 184;f185=f 185;f186=f 186;f187=f 187;f188=i32 188;f189=i64 189;
  f190=f 190;f191=f 191;f192=i_n 192;f193=f 193;f194=f 194;f195=f 195;f196=f 196;
  f197=f 197;f198=i32 198;f199=i64 199;f200=f 200;f201=f 201;f202=i_n 202;f203=f 203;
  f204=f 204;f205=f 205;f206=f 206;f207=f 207;f208=i32 208;f209=i64 209;f210=f 210;
  f211=f 211;f212=i_n 212;f213=f 213;f214=f 214;f215=f 215;f216=f 216;f217=f 217;
  f218=i32 218;f219=i64 219;f220=f 220;f221=f 221;f222=i_n 222;f223=f 223;f224=f 224;
  f225=f 225;f226=f 226;f227=f 227;f228=i32 228;f229=i64 229;f230=f 230;f231=f 231;
  f232=i_n 232;f233=f 233;f234=f 234;f235=f 235;f236=f 236;f237=f 237;f238=i32 238;
  f239=i64 239;f240=f 240;f241=f 241;f242=i_n 242;f243=f 243;f244=f 244;f245=f 245;
  f246=f 246;f247=f 247;f248=i32 248;f249=i64 249;f250=f 250;f251=f 251;f252=i_n 252;
  f253=f 253;f254=f 254;f255=f 255;f256=f 256;f257=f 257;f258=i32 258;f259=i64 259;
  }

let iter_t4 f i32 i64 i_n t4 =
  f t4.f1 1; i_n t4.f2 2; f t4.f3 3; f t4.f4 4; f t4.f5 5; f t4.f6 6; f t4.f7 7;
    i32 t4.f8 8; i64 t4.f9 9; f t4.f10 10; f t4.f11 11;
    i_n t4.f12 12; f t4.f13 13; f t4.f14 14;
    f t4.f15 15; f t4.f16 16; f t4.f17 17; i32 t4.f18 18;
    i64 t4.f19 19; f t4.f20 20; f t4.f21 21;
    i_n t4.f22 22; f t4.f23 23; f t4.f24 24; f t4.f25 25;
    f t4.f26 26; f t4.f27 27; i32 t4.f28 28;
    i64 t4.f29 29; f t4.f30 30; f t4.f31 31; i_n t4.f32 32;
    f t4.f33 33; f t4.f34 34; f t4.f35 35;
    f t4.f36 36; f t4.f37 37; i32 t4.f38 38; i64 t4.f39 39;
    f t4.f40 40; f t4.f41 41; i_n t4.f42 42;
    f t4.f43 43; f t4.f44 44; f t4.f45 45; f t4.f46 46;
    f t4.f47 47; i32 t4.f48 48; i64 t4.f49 49;
    f t4.f50 50; f t4.f51 51; i_n t4.f52 52; f t4.f53 53;
    f t4.f54 54; f t4.f55 55; f t4.f56 56;
    f t4.f57 57; i32 t4.f58 58; i64 t4.f59 59; f t4.f60 60;
    f t4.f61 61; i_n t4.f62 62; f t4.f63 63;
    f t4.f64 64; f t4.f65 65; f t4.f66 66; f t4.f67 67;
    i32 t4.f68 68; i64 t4.f69 69; f t4.f70 70;
    f t4.f71 71; i_n t4.f72 72; f t4.f73 73; f t4.f74 74;
    f t4.f75 75; f t4.f76 76; f t4.f77 77;
    i32 t4.f78 78; i64 t4.f79 79; f t4.f80 80; f t4.f81 81;
    i_n t4.f82 82; f t4.f83 83; f t4.f84 84;
    f t4.f85 85; f t4.f86 86; f t4.f87 87; i32 t4.f88 88;
    i64 t4.f89 89; f t4.f90 90; f t4.f91 91;
    i_n t4.f92 92; f t4.f93 93; f t4.f94 94; f t4.f95 95;
    f t4.f96 96; f t4.f97 97; i32 t4.f98 98;
    i64 t4.f99 99; f t4.f100 100; f t4.f101 101; i_n t4.f102 102;
    f t4.f103 103; f t4.f104 104; f t4.f105 105;
    f t4.f106 106; f t4.f107 107; i32 t4.f108 108; i64 t4.f109 109;
    f t4.f110 110; f t4.f111 111; i_n t4.f112 112;
    f t4.f113 113; f t4.f114 114; f t4.f115 115; f t4.f116 116;
    f t4.f117 117; i32 t4.f118 118; i64 t4.f119 119;
    f t4.f120 120; f t4.f121 121; i_n t4.f122 122; f t4.f123 123;
    f t4.f124 124; f t4.f125 125; f t4.f126 126;
    f t4.f127 127; i32 t4.f128 128; i64 t4.f129 129; f t4.f130 130;
    f t4.f131 131; i_n t4.f132 132; f t4.f133 133;
    f t4.f134 134; f t4.f135 135; f t4.f136 136; f t4.f137 137;
    i32 t4.f138 138; i64 t4.f139 139; f t4.f140 140;
    f t4.f141 141; i_n t4.f142 142; f t4.f143 143; f t4.f144 144;
    f t4.f145 145; f t4.f146 146; f t4.f147 147;
    i32 t4.f148 148; i64 t4.f149 149; f t4.f150 150; f t4.f151 151;
    i_n t4.f152 152; f t4.f153 153; f t4.f154 154;
    f t4.f155 155; f t4.f156 156; f t4.f157 157; i32 t4.f158 158;
    i64 t4.f159 159; f t4.f160 160; f t4.f161 161;
    i_n t4.f162 162; f t4.f163 163; f t4.f164 164; f t4.f165 165;
    f t4.f166 166; f t4.f167 167; i32 t4.f168 168;
    i64 t4.f169 169; f t4.f170 170; f t4.f171 171; i_n t4.f172 172;
    f t4.f173 173; f t4.f174 174; f t4.f175 175;
    f t4.f176 176; f t4.f177 177; i32 t4.f178 178; i64 t4.f179 179;
    f t4.f180 180; f t4.f181 181; i_n t4.f182 182;
    f t4.f183 183; f t4.f184 184; f t4.f185 185; f t4.f186 186;
    f t4.f187 187; i32 t4.f188 188; i64 t4.f189 189;
    f t4.f190 190; f t4.f191 191; i_n t4.f192 192; f t4.f193 193;
    f t4.f194 194; f t4.f195 195; f t4.f196 196;
    f t4.f197 197; i32 t4.f198 198; i64 t4.f199 199; f t4.f200 200;
    f t4.f201 201; i_n t4.f202 202; f t4.f203 203;
    f t4.f204 204; f t4.f205 205; f t4.f206 206; f t4.f207 207;
    i32 t4.f208 208; i64 t4.f209 209; f t4.f210 210;
    f t4.f211 211; i_n t4.f212 212; f t4.f213 213; f t4.f214 214;
    f t4.f215 215; f t4.f216 216; f t4.f217 217;
    i32 t4.f218 218; i64 t4.f219 219; f t4.f220 220; f t4.f221 221;
    i_n t4.f222 222; f t4.f223 223; f t4.f224 224;
    f t4.f225 225; f t4.f226 226; f t4.f227 227; i32 t4.f228 228;
    i64 t4.f229 229; f t4.f230 230; f t4.f231 231;
    i_n t4.f232 232; f t4.f233 233; f t4.f234 234; f t4.f235 235;
    f t4.f236 236; f t4.f237 237; i32 t4.f238 238;
    i64 t4.f239 239; f t4.f240 240; f t4.f241 241; i_n t4.f242 242;
    f t4.f243 243; f t4.f244 244; f t4.f245 245;
    f t4.f246 246; f t4.f247 247; i32 t4.f248 248; i64 t4.f249 249;
    f t4.f250 250; f t4.f251 251; i_n t4.f252 252;
    f t4.f253 253; f t4.f254 254; f t4.f255 255; f t4.f256 256;
    f t4.f257 257; i32 t4.f258 258; i64 t4.f259 259;
;;

let test4 () =
  let t =
    create_t4 Float_u.of_int Int32_u.of_int Int64_u.of_int Nativeint_u.of_int
  in
  let sum () =
    let total = ref 0.0 in
    iter_t4
      (fun f _ -> total := !total +. Float_u.to_float f)
      (fun i _ -> total := !total +. Int32_u.to_float i)
      (fun i _ -> total := !total +. Int64_u.to_float i)
      (fun i _ -> total := !total +. Nativeint_u.to_float i)
      t;
    !total
  in
  Printf.printf "Test 4: sum (33670) = %.3f\n" (sum ());
  let check () =
    iter_t4
      (fun f i -> assert (Float_u.equal f (Float_u.of_int i)))
      (fun i32 i -> assert (Int32_u.equal i32 (Int32_u.of_int i)))
      (fun i64 i -> assert (Int64_u.equal i64 (Int64_u.of_int i)))
      (fun i_n i -> assert (Nativeint_u.equal i_n (Nativeint_u.of_int i)))
      t
  in
  check ();
  Gc.full_major ();
  check ();
  Printf.printf "  at end, sum (33670) = %.3f\n" (sum ());
;;

let () = test4 ()

(*************************)
(* Test 5: optimizations *)

(* Test that the middle end is able to handle scenarios where
   optimizations apply. This doesn't give us assurance that
   the optimizations actually apply here -- just that the middle-end
   doesn't fall over.
*)

type t5_1 =
  { imm : int; fl : float#; i32 : int32#; i64 : int64#; i_n : nativeint# }

let construct_and_destruct_1_1 imm fl i32 i64 i_n =
  match { imm; fl; i32; i64; i_n } with
  | { imm; fl; i32; i64; i_n } ->
      float_of_int imm
      +. Float_u.to_float fl
      +. Int32_u.to_float i32
      +. Int64_u.to_float i64
      +. Nativeint_u.to_float i_n


let construct_and_destruct_1_2 b imm fl i32 i64 i_n =
  let { imm; fl; i32; i64; i_n } =
    match b with
    | true ->  { imm; fl; i32; i64; i_n }
    | false -> { imm = imm + 1; fl; i32; i64; i_n }
  in
  float_of_int imm
  +. Float_u.to_float fl
  +. Int32_u.to_float i32
  +. Int64_u.to_float i64
  +. Nativeint_u.to_float i_n

type t5_2 = { x : float; y : float# }

let construct_and_destruct_2_1 ~x ~y =
  match { x; y } with
  | { x; y } -> x +. Float_u.to_float y

let construct_and_destruct_2_2 b ~x ~y =
  let { x; y } =
    match b with
    | true -> { x; y }
    | false -> { x = x +. 1.; y }
  in
  x +. Float_u.to_float y

let test5 () =
  let result1_1 = construct_and_destruct_1_1 36 #4. (-#10l) #13L (-#1n) in
  let result1_2 = construct_and_destruct_1_2 true 38 #4. #255l (-#435L) #180n in
  let result2_1 = construct_and_destruct_2_1 ~x:37. ~y:#5. in
  let result2_2 = construct_and_destruct_2_2 false ~x:36. ~y:#5. in
  Printf.printf "Test 5: result (42) = %.3f = %.3f = %.3f = %.3f\n"
    result1_1 result1_2 result2_1 result2_2
;;

let () = test5 ()
