(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(*****************************************)
(* Prelude: Functions on unboxed numbers *)

module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u

(******************************)
(* Test: large mixed blocks *)

(* Allocations of large-enough blocks go through a different code path in the
   backend and runtime. This mixed block is larger than that threshold.
 *)

type uf = float#
type i32 = int32#
type i64 = int64#
type i_n = nativeint#
type t_large =
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

let create_large f i32 i64 i_n =
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

let iter_large f i32 i64 i_n t =
  f t.f1 1; i_n t.f2 2; f t.f3 3; f t.f4 4; f t.f5 5; f t.f6 6; f t.f7 7;
    i32 t.f8 8; i64 t.f9 9; f t.f10 10; f t.f11 11;
    i_n t.f12 12; f t.f13 13; f t.f14 14;
    f t.f15 15; f t.f16 16; f t.f17 17; i32 t.f18 18;
    i64 t.f19 19; f t.f20 20; f t.f21 21;
    i_n t.f22 22; f t.f23 23; f t.f24 24; f t.f25 25;
    f t.f26 26; f t.f27 27; i32 t.f28 28;
    i64 t.f29 29; f t.f30 30; f t.f31 31; i_n t.f32 32;
    f t.f33 33; f t.f34 34; f t.f35 35;
    f t.f36 36; f t.f37 37; i32 t.f38 38; i64 t.f39 39;
    f t.f40 40; f t.f41 41; i_n t.f42 42;
    f t.f43 43; f t.f44 44; f t.f45 45; f t.f46 46;
    f t.f47 47; i32 t.f48 48; i64 t.f49 49;
    f t.f50 50; f t.f51 51; i_n t.f52 52; f t.f53 53;
    f t.f54 54; f t.f55 55; f t.f56 56;
    f t.f57 57; i32 t.f58 58; i64 t.f59 59; f t.f60 60;
    f t.f61 61; i_n t.f62 62; f t.f63 63;
    f t.f64 64; f t.f65 65; f t.f66 66; f t.f67 67;
    i32 t.f68 68; i64 t.f69 69; f t.f70 70;
    f t.f71 71; i_n t.f72 72; f t.f73 73; f t.f74 74;
    f t.f75 75; f t.f76 76; f t.f77 77;
    i32 t.f78 78; i64 t.f79 79; f t.f80 80; f t.f81 81;
    i_n t.f82 82; f t.f83 83; f t.f84 84;
    f t.f85 85; f t.f86 86; f t.f87 87; i32 t.f88 88;
    i64 t.f89 89; f t.f90 90; f t.f91 91;
    i_n t.f92 92; f t.f93 93; f t.f94 94; f t.f95 95;
    f t.f96 96; f t.f97 97; i32 t.f98 98;
    i64 t.f99 99; f t.f100 100; f t.f101 101; i_n t.f102 102;
    f t.f103 103; f t.f104 104; f t.f105 105;
    f t.f106 106; f t.f107 107; i32 t.f108 108; i64 t.f109 109;
    f t.f110 110; f t.f111 111; i_n t.f112 112;
    f t.f113 113; f t.f114 114; f t.f115 115; f t.f116 116;
    f t.f117 117; i32 t.f118 118; i64 t.f119 119;
    f t.f120 120; f t.f121 121; i_n t.f122 122; f t.f123 123;
    f t.f124 124; f t.f125 125; f t.f126 126;
    f t.f127 127; i32 t.f128 128; i64 t.f129 129; f t.f130 130;
    f t.f131 131; i_n t.f132 132; f t.f133 133;
    f t.f134 134; f t.f135 135; f t.f136 136; f t.f137 137;
    i32 t.f138 138; i64 t.f139 139; f t.f140 140;
    f t.f141 141; i_n t.f142 142; f t.f143 143; f t.f144 144;
    f t.f145 145; f t.f146 146; f t.f147 147;
    i32 t.f148 148; i64 t.f149 149; f t.f150 150; f t.f151 151;
    i_n t.f152 152; f t.f153 153; f t.f154 154;
    f t.f155 155; f t.f156 156; f t.f157 157; i32 t.f158 158;
    i64 t.f159 159; f t.f160 160; f t.f161 161;
    i_n t.f162 162; f t.f163 163; f t.f164 164; f t.f165 165;
    f t.f166 166; f t.f167 167; i32 t.f168 168;
    i64 t.f169 169; f t.f170 170; f t.f171 171; i_n t.f172 172;
    f t.f173 173; f t.f174 174; f t.f175 175;
    f t.f176 176; f t.f177 177; i32 t.f178 178; i64 t.f179 179;
    f t.f180 180; f t.f181 181; i_n t.f182 182;
    f t.f183 183; f t.f184 184; f t.f185 185; f t.f186 186;
    f t.f187 187; i32 t.f188 188; i64 t.f189 189;
    f t.f190 190; f t.f191 191; i_n t.f192 192; f t.f193 193;
    f t.f194 194; f t.f195 195; f t.f196 196;
    f t.f197 197; i32 t.f198 198; i64 t.f199 199; f t.f200 200;
    f t.f201 201; i_n t.f202 202; f t.f203 203;
    f t.f204 204; f t.f205 205; f t.f206 206; f t.f207 207;
    i32 t.f208 208; i64 t.f209 209; f t.f210 210;
    f t.f211 211; i_n t.f212 212; f t.f213 213; f t.f214 214;
    f t.f215 215; f t.f216 216; f t.f217 217;
    i32 t.f218 218; i64 t.f219 219; f t.f220 220; f t.f221 221;
    i_n t.f222 222; f t.f223 223; f t.f224 224;
    f t.f225 225; f t.f226 226; f t.f227 227; i32 t.f228 228;
    i64 t.f229 229; f t.f230 230; f t.f231 231;
    i_n t.f232 232; f t.f233 233; f t.f234 234; f t.f235 235;
    f t.f236 236; f t.f237 237; i32 t.f238 238;
    i64 t.f239 239; f t.f240 240; f t.f241 241; i_n t.f242 242;
    f t.f243 243; f t.f244 244; f t.f245 245;
    f t.f246 246; f t.f247 247; i32 t.f248 248; i64 t.f249 249;
    f t.f250 250; f t.f251 251; i_n t.f252 252;
    f t.f253 253; f t.f254 254; f t.f255 255; f t.f256 256;
    f t.f257 257; i32 t.f258 258; i64 t.f259 259;
;;

let test_large_mixed_blocks () =
  let t =
    create_large Float_u.of_int Int32_u.of_int Int64_u.of_int Nativeint_u.of_int
  in
  let sum () =
    let total = ref 0.0 in
    iter_large
      (fun f _ -> total := !total +. Float_u.to_float f)
      (fun i _ -> total := !total +. Int32_u.to_float i)
      (fun i _ -> total := !total +. Int64_u.to_float i)
      (fun i _ -> total := !total +. Nativeint_u.to_float i)
      t;
    !total
  in
  Printf.printf "Test large mixed blocks: sum (33670) = %.3f\n" (sum ());
  let check () =
    iter_large
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

let () = test_large_mixed_blocks ()

(*************************)
(* Test 5: optimizations *)

(* Test that the middle end is able to handle scenarios where
   optimizations apply. This doesn't give us assurance that
   the optimizations actually apply here -- just that the middle-end
   doesn't fall over.
*)

type t_opt1 =
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

type t_opt2 = { x : float; y : float# }

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

let test_opt () =
  let result1_1 = construct_and_destruct_1_1 36 #4. (-#10l) #13L (-#1n) in
  let result1_2 = construct_and_destruct_1_2 true 38 #4. #255l (-#435L) #180n in
  let result2_1 = construct_and_destruct_2_1 ~x:37. ~y:#5. in
  let result2_2 = construct_and_destruct_2_2 false ~x:36. ~y:#5. in
  Printf.printf "Test opt: result (42) = %.3f = %.3f = %.3f = %.3f\n"
    result1_1 result1_2 result2_1 result2_2
;;

let () = test_opt ()
