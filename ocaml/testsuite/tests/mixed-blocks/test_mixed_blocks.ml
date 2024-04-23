(* TEST
 reference = "${test_source_directory}/test_mixed_blocks.reference";
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
type t4 =
  { x : string;
    f1 : uf; f2 : uf; f3 : uf; f4 : uf; f5 : uf; f6 : uf; f7 : uf;
    f8 : uf; f9 : uf; f10 : uf; f11 : uf; f12 : uf; f13 : uf; f14 : uf;
    f15 : uf; f16 : uf; f17 : uf; f18 : uf; f19 : uf; f20 : uf; f21 : uf;
    f22 : uf; f23 : uf; f24 : uf; f25 : uf; f26 : uf; f27 : uf; f28 : uf;
    f29 : uf; f30 : uf; f31 : uf; f32 : uf; f33 : uf; f34 : uf; f35 : uf;
    f36 : uf; f37 : uf; f38 : uf; f39 : uf; f40 : uf; f41 : uf; f42 : uf;
    f43 : uf; f44 : uf; f45 : uf; f46 : uf; f47 : uf; f48 : uf; f49 : uf;
    f50 : uf; f51 : uf; f52 : uf; f53 : uf; f54 : uf; f55 : uf; f56 : uf;
    f57 : uf; f58 : uf; f59 : uf; f60 : uf; f61 : uf; f62 : uf; f63 : uf;
    f64 : uf; f65 : uf; f66 : uf; f67 : uf; f68 : uf; f69 : uf; f70 : uf;
    f71 : uf; f72 : uf; f73 : uf; f74 : uf; f75 : uf; f76 : uf; f77 : uf;
    f78 : uf; f79 : uf; f80 : uf; f81 : uf; f82 : uf; f83 : uf; f84 : uf;
    f85 : uf; f86 : uf; f87 : uf; f88 : uf; f89 : uf; f90 : uf; f91 : uf;
    f92 : uf; f93 : uf; f94 : uf; f95 : uf; f96 : uf; f97 : uf; f98 : uf;
    f99 : uf; f100 : uf; f101 : uf; f102 : uf; f103 : uf; f104 : uf; f105 : uf;
    f106 : uf; f107 : uf; f108 : uf; f109 : uf; f110 : uf; f111 : uf; f112 : uf;
    f113 : uf; f114 : uf; f115 : uf; f116 : uf; f117 : uf; f118 : uf; f119 : uf;
    f120 : uf; f121 : uf; f122 : uf; f123 : uf; f124 : uf; f125 : uf; f126 : uf;
    f127 : uf; f128 : uf; f129 : uf; f130 : uf; f131 : uf; f132 : uf; f133 : uf;
    f134 : uf; f135 : uf; f136 : uf; f137 : uf; f138 : uf; f139 : uf; f140 : uf;
    f141 : uf; f142 : uf; f143 : uf; f144 : uf; f145 : uf; f146 : uf; f147 : uf;
    f148 : uf; f149 : uf; f150 : uf; f151 : uf; f152 : uf; f153 : uf; f154 : uf;
    f155 : uf; f156 : uf; f157 : uf; f158 : uf; f159 : uf; f160 : uf; f161 : uf;
    f162 : uf; f163 : uf; f164 : uf; f165 : uf; f166 : uf; f167 : uf; f168 : uf;
    f169 : uf; f170 : uf; f171 : uf; f172 : uf; f173 : uf; f174 : uf; f175 : uf;
    f176 : uf; f177 : uf; f178 : uf; f179 : uf; f180 : uf; f181 : uf; f182 : uf;
    f183 : uf; f184 : uf; f185 : uf; f186 : uf; f187 : uf; f188 : uf; f189 : uf;
    f190 : uf; f191 : uf; f192 : uf; f193 : uf; f194 : uf; f195 : uf; f196 : uf;
    f197 : uf; f198 : uf; f199 : uf; f200 : uf; f201 : uf; f202 : uf; f203 : uf;
    f204 : uf; f205 : uf; f206 : uf; f207 : uf; f208 : uf; f209 : uf; f210 : uf;
    f211 : uf; f212 : uf; f213 : uf; f214 : uf; f215 : uf; f216 : uf; f217 : uf;
    f218 : uf; f219 : uf; f220 : uf; f221 : uf; f222 : uf; f223 : uf; f224 : uf;
    f225 : uf; f226 : uf; f227 : uf; f228 : uf; f229 : uf; f230 : uf; f231 : uf;
    f232 : uf; f233 : uf; f234 : uf; f235 : uf; f236 : uf; f237 : uf; f238 : uf;
    f239 : uf; f240 : uf; f241 : uf; f242 : uf; f243 : uf; f244 : uf; f245 : uf;
    f246 : uf; f247 : uf; f248 : uf; f249 : uf; f250 : uf; f251 : uf; f252 : uf;
    f253 : uf; f254 : uf; f255 : uf; f256 : uf; f257 : uf; f258 : uf; f259 : uf;
  }

let create_t4 f =
  { x = "prefix";
  f1=f 1;f2=f 2;f3=f 3;f4=f 4;f5=f 5;f6=f 6;f7=f 7;
  f8=f 8;f9=f 9;f10=f 10;f11=f 11;f12=f 12;f13=f 13;f14=f 14;
  f15=f 15;f16=f 16;f17=f 17;f18=f 18;f19=f 19;f20=f 20;f21=f 21;
  f22=f 22;f23=f 23;f24=f 24;f25=f 25;f26=f 26;f27=f 27;f28=f 28;
  f29=f 29;f30=f 30;f31=f 31;f32=f 32;f33=f 33;f34=f 34;f35=f 35;
  f36=f 36;f37=f 37;f38=f 38;f39=f 39;f40=f 40;f41=f 41;f42=f 42;
  f43=f 43;f44=f 44;f45=f 45;f46=f 46;f47=f 47;f48=f 48;f49=f 49;
  f50=f 50;f51=f 51;f52=f 52;f53=f 53;f54=f 54;f55=f 55;f56=f 56;
  f57=f 57;f58=f 58;f59=f 59;f60=f 60;f61=f 61;f62=f 62;f63=f 63;
  f64=f 64;f65=f 65;f66=f 66;f67=f 67;f68=f 68;f69=f 69;f70=f 70;
  f71=f 71;f72=f 72;f73=f 73;f74=f 74;f75=f 75;f76=f 76;f77=f 77;
  f78=f 78;f79=f 79;f80=f 80;f81=f 81;f82=f 82;f83=f 83;f84=f 84;
  f85=f 85;f86=f 86;f87=f 87;f88=f 88;f89=f 89;f90=f 90;f91=f 91;
  f92=f 92;f93=f 93;f94=f 94;f95=f 95;f96=f 96;f97=f 97;f98=f 98;
  f99=f 99;f100=f 100;f101=f 101;f102=f 102;f103=f 103;f104=f 104;f105=f 105;
  f106=f 106;f107=f 107;f108=f 108;f109=f 109;f110=f 110;f111=f 111;f112=f 112;
  f113=f 113;f114=f 114;f115=f 115;f116=f 116;f117=f 117;f118=f 118;f119=f 119;
  f120=f 120;f121=f 121;f122=f 122;f123=f 123;f124=f 124;f125=f 125;f126=f 126;
  f127=f 127;f128=f 128;f129=f 129;f130=f 130;f131=f 131;f132=f 132;f133=f 133;
  f134=f 134;f135=f 135;f136=f 136;f137=f 137;f138=f 138;f139=f 139;f140=f 140;
  f141=f 141;f142=f 142;f143=f 143;f144=f 144;f145=f 145;f146=f 146;f147=f 147;
  f148=f 148;f149=f 149;f150=f 150;f151=f 151;f152=f 152;f153=f 153;f154=f 154;
  f155=f 155;f156=f 156;f157=f 157;f158=f 158;f159=f 159;f160=f 160;f161=f 161;
  f162=f 162;f163=f 163;f164=f 164;f165=f 165;f166=f 166;f167=f 167;f168=f 168;
  f169=f 169;f170=f 170;f171=f 171;f172=f 172;f173=f 173;f174=f 174;f175=f 175;
  f176=f 176;f177=f 177;f178=f 178;f179=f 179;f180=f 180;f181=f 181;f182=f 182;
  f183=f 183;f184=f 184;f185=f 185;f186=f 186;f187=f 187;f188=f 188;f189=f 189;
  f190=f 190;f191=f 191;f192=f 192;f193=f 193;f194=f 194;f195=f 195;f196=f 196;
  f197=f 197;f198=f 198;f199=f 199;f200=f 200;f201=f 201;f202=f 202;f203=f 203;
  f204=f 204;f205=f 205;f206=f 206;f207=f 207;f208=f 208;f209=f 209;f210=f 210;
  f211=f 211;f212=f 212;f213=f 213;f214=f 214;f215=f 215;f216=f 216;f217=f 217;
  f218=f 218;f219=f 219;f220=f 220;f221=f 221;f222=f 222;f223=f 223;f224=f 224;
  f225=f 225;f226=f 226;f227=f 227;f228=f 228;f229=f 229;f230=f 230;f231=f 231;
  f232=f 232;f233=f 233;f234=f 234;f235=f 235;f236=f 236;f237=f 237;f238=f 238;
  f239=f 239;f240=f 240;f241=f 241;f242=f 242;f243=f 243;f244=f 244;f245=f 245;
  f246=f 246;f247=f 247;f248=f 248;f249=f 249;f250=f 250;f251=f 251;f252=f 252;
  f253=f 253;f254=f 254;f255=f 255;f256=f 256;f257=f 257;f258=f 258;f259=f 259;
  }

let iter_t4 f t4 =
  f t4.f1 1; f t4.f2 2; f t4.f3 3; f t4.f4 4; f t4.f5 5; f t4.f6 6; f t4.f7 7;
    f t4.f8 8; f t4.f9 9; f t4.f10 10; f t4.f11 11;
    f t4.f12 12; f t4.f13 13; f t4.f14 14;
    f t4.f15 15; f t4.f16 16; f t4.f17 17; f t4.f18 18;
    f t4.f19 19; f t4.f20 20; f t4.f21 21;
    f t4.f22 22; f t4.f23 23; f t4.f24 24; f t4.f25 25;
    f t4.f26 26; f t4.f27 27; f t4.f28 28;
    f t4.f29 29; f t4.f30 30; f t4.f31 31; f t4.f32 32;
    f t4.f33 33; f t4.f34 34; f t4.f35 35;
    f t4.f36 36; f t4.f37 37; f t4.f38 38; f t4.f39 39;
    f t4.f40 40; f t4.f41 41; f t4.f42 42;
    f t4.f43 43; f t4.f44 44; f t4.f45 45; f t4.f46 46;
    f t4.f47 47; f t4.f48 48; f t4.f49 49;
    f t4.f50 50; f t4.f51 51; f t4.f52 52; f t4.f53 53;
    f t4.f54 54; f t4.f55 55; f t4.f56 56;
    f t4.f57 57; f t4.f58 58; f t4.f59 59; f t4.f60 60;
    f t4.f61 61; f t4.f62 62; f t4.f63 63;
    f t4.f64 64; f t4.f65 65; f t4.f66 66; f t4.f67 67;
    f t4.f68 68; f t4.f69 69; f t4.f70 70;
    f t4.f71 71; f t4.f72 72; f t4.f73 73; f t4.f74 74;
    f t4.f75 75; f t4.f76 76; f t4.f77 77;
    f t4.f78 78; f t4.f79 79; f t4.f80 80; f t4.f81 81;
    f t4.f82 82; f t4.f83 83; f t4.f84 84;
    f t4.f85 85; f t4.f86 86; f t4.f87 87; f t4.f88 88;
    f t4.f89 89; f t4.f90 90; f t4.f91 91;
    f t4.f92 92; f t4.f93 93; f t4.f94 94; f t4.f95 95;
    f t4.f96 96; f t4.f97 97; f t4.f98 98;
    f t4.f99 99; f t4.f100 100; f t4.f101 101; f t4.f102 102;
    f t4.f103 103; f t4.f104 104; f t4.f105 105;
    f t4.f106 106; f t4.f107 107; f t4.f108 108; f t4.f109 109;
    f t4.f110 110; f t4.f111 111; f t4.f112 112;
    f t4.f113 113; f t4.f114 114; f t4.f115 115; f t4.f116 116;
    f t4.f117 117; f t4.f118 118; f t4.f119 119;
    f t4.f120 120; f t4.f121 121; f t4.f122 122; f t4.f123 123;
    f t4.f124 124; f t4.f125 125; f t4.f126 126;
    f t4.f127 127; f t4.f128 128; f t4.f129 129; f t4.f130 130;
    f t4.f131 131; f t4.f132 132; f t4.f133 133;
    f t4.f134 134; f t4.f135 135; f t4.f136 136; f t4.f137 137;
    f t4.f138 138; f t4.f139 139; f t4.f140 140;
    f t4.f141 141; f t4.f142 142; f t4.f143 143; f t4.f144 144;
    f t4.f145 145; f t4.f146 146; f t4.f147 147;
    f t4.f148 148; f t4.f149 149; f t4.f150 150; f t4.f151 151;
    f t4.f152 152; f t4.f153 153; f t4.f154 154;
    f t4.f155 155; f t4.f156 156; f t4.f157 157; f t4.f158 158;
    f t4.f159 159; f t4.f160 160; f t4.f161 161;
    f t4.f162 162; f t4.f163 163; f t4.f164 164; f t4.f165 165;
    f t4.f166 166; f t4.f167 167; f t4.f168 168;
    f t4.f169 169; f t4.f170 170; f t4.f171 171; f t4.f172 172;
    f t4.f173 173; f t4.f174 174; f t4.f175 175;
    f t4.f176 176; f t4.f177 177; f t4.f178 178; f t4.f179 179;
    f t4.f180 180; f t4.f181 181; f t4.f182 182;
    f t4.f183 183; f t4.f184 184; f t4.f185 185; f t4.f186 186;
    f t4.f187 187; f t4.f188 188; f t4.f189 189;
    f t4.f190 190; f t4.f191 191; f t4.f192 192; f t4.f193 193;
    f t4.f194 194; f t4.f195 195; f t4.f196 196;
    f t4.f197 197; f t4.f198 198; f t4.f199 199; f t4.f200 200;
    f t4.f201 201; f t4.f202 202; f t4.f203 203;
    f t4.f204 204; f t4.f205 205; f t4.f206 206; f t4.f207 207;
    f t4.f208 208; f t4.f209 209; f t4.f210 210;
    f t4.f211 211; f t4.f212 212; f t4.f213 213; f t4.f214 214;
    f t4.f215 215; f t4.f216 216; f t4.f217 217;
    f t4.f218 218; f t4.f219 219; f t4.f220 220; f t4.f221 221;
    f t4.f222 222; f t4.f223 223; f t4.f224 224;
    f t4.f225 225; f t4.f226 226; f t4.f227 227; f t4.f228 228;
    f t4.f229 229; f t4.f230 230; f t4.f231 231;
    f t4.f232 232; f t4.f233 233; f t4.f234 234; f t4.f235 235;
    f t4.f236 236; f t4.f237 237; f t4.f238 238;
    f t4.f239 239; f t4.f240 240; f t4.f241 241; f t4.f242 242;
    f t4.f243 243; f t4.f244 244; f t4.f245 245;
    f t4.f246 246; f t4.f247 247; f t4.f248 248; f t4.f249 249;
    f t4.f250 250; f t4.f251 251; f t4.f252 252;
    f t4.f253 253; f t4.f254 254; f t4.f255 255; f t4.f256 256;
    f t4.f257 257; f t4.f258 258; f t4.f259 259;
;;

let test4 () =
  let t = create_t4 Float_u.of_int in
  let sum () =
    let total = ref 0.0 in
    iter_t4 (fun f _ ->
      total := !total +. Float_u.to_float f) t;
    !total
  in
  Printf.printf "Test 4: sum (33670) = %.3f\n" (sum ());
  iter_t4 (fun f i ->
    assert (Float_u.equal f (Float_u.of_int i))) t;
  Gc.full_major ();
  iter_t4 (fun f i ->
    assert (Float_u.equal f (Float_u.of_int i))) t;
  Printf.printf "  at end, sum (33670) = %.3f\n" (sum ());
;;

let () = test4 ()
