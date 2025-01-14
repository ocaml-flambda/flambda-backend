(* TEST *)

[@@@ocaml.flambda_oclassic]

external int_as_pointer : _ -> int = "%int_as_pointer"

module Int64_u = struct
  external to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
    [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"
    [@@warning "-187"]

  let equal x y = Int64.equal (to_int64 x) (to_int64 y)
end

let[@opaque] rand_near_minor_heap () =
  let r = ref () in
  let i : int = Obj.magic (int_as_pointer r) in
  let minor_heap_size_in_bytes =
    assert (Sys.word_size = 64);
    (Gc.get ()).minor_heap_size * Sys.word_size / 8
  in
  let b = minor_heap_size_in_bytes / 2 in
  let n = (Random.int b - (b / 2)) * 2 in
  Int64_u.of_int64 (Int64.of_int ((i * 2) + n))

let[@opaque] rand_string () =
  match Random.int 3 with
  | 0 -> "goat"
  | 1 -> "sheep"
  | 2 -> "cow"
  | _ -> assert false

(* Example flambda2 output:

(c1_1arg/870UV) =
  (set_of_closures Heap
  ({((c1_1arg/4 ∷ 𝕍*|Null) camlObj_dup_closures__c1_1arg_4_22_code)})
  (env {((i64_1/0 ∷ ℕ𝟞𝟜) i64_1/864UV)}))
(c2_1arg/879UV) =
  (set_of_closures Heap
  ({((c2_1arg/5 ∷ 𝕍*|Null) camlObj_dup_closures__c2_1arg_5_23_code)})
  (env {((i64_2/1 ∷ ℕ𝟞𝟜) i64_2/865UV) ((x/2 ∷ 𝕍) x/868UV)}))
(c3_1arg/892UV) =
  (set_of_closures Heap
  ({((c3_1arg/6 ∷ 𝕍*|Null) camlObj_dup_closures__c3_1arg_6_24_code)})
  (env {((x/3 ∷ 𝕍) x/868UV)}))
(c1_2arg/904UV) =
  (set_of_closures Heap
  ({((c1_2arg/7 ∷ 𝕍*|Null) camlObj_dup_closures__c1_2arg_7_25_code)})
  (env {((i64_3/4 ∷ ℕ𝟞𝟜) i64_3/866UV)}))
(c2_2arg/914UV) =
  (set_of_closures Heap
  ({((c2_2arg/8 ∷ 𝕍*|Null) camlObj_dup_closures__c2_2arg_8_26_code)})
  (env {((i64_4/5 ∷ ℕ𝟞𝟜) i64_4/867UV) ((x/6 ∷ 𝕍) x/868UV)}))
(c3_2arg/928UV) =
  (set_of_closures Heap
  ({((c3_2arg/9 ∷ 𝕍*|Null) camlObj_dup_closures__c3_2arg_9_27_code)})
  (env {((x/7 ∷ 𝕍) x/868UV)}))
(rec_c1_1arg/941UV rec_c2_1arg/942UV rec_c3_1arg/943UV rec_c1_2arg/944UV
  rec_c2_2arg/945UV rec_c3_2arg/946UV) =
  (set_of_closures Heap
  ({((rec_c1_1arg/10 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c1_1arg_10_28_code)
    ((rec_c2_1arg/11 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c2_1arg_11_29_code)
    ((rec_c3_1arg/12 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c3_1arg_12_30_code)
    ((rec_c1_2arg/13 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c1_2arg_13_31_code)
    ((rec_c2_2arg/14 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c2_2arg_14_32_code)
    ((rec_c3_2arg/15 ∷ 𝕍*|Null) camlObj_dup_closures__rec_c3_2arg_15_33_code)})
  (env
    {((i64_1/8 ∷ ℕ𝟞𝟜) i64_1/864UV) ((i64_2/9 ∷ ℕ𝟞𝟜) i64_2/865UV)
    ((i64_3/10 ∷ ℕ𝟞𝟜) i64_3/866UV) ((i64_4/11 ∷ ℕ𝟞𝟜) i64_4/867UV)
    ((x/12 ∷ 𝕍) x/868UV)}))
*)

let[@opaque] make_small_closures (i64_1 : int64#) (i64_2 : int64#)
    (i64_3 : int64#) (i64_4 : int64#) (x : string) =
  (* Two-word function slot (i.e. one argument) cases *)
  let[@opaque] c1_1arg () =
    (* Only an unboxed environment *)
    i64_1
  in
  let[@opaque] c2_1arg () =
    (* An unboxed environment plus a scannable environment *)
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_2
  in
  let[@opaque] c3_1arg () =
    (* Only a scannable environment *)
    let (_i : int) = Sys.opaque_identity (String.length x) in
    100
  in
  (* Three-word function slot (i.e. more than one argument) cases *)
  let[@opaque] c1_2arg () () = i64_3 in
  let[@opaque] c2_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_4
  in
  let[@opaque] c3_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    200
  in
  (* Cases to exercise [Infix_tag] logic *)
  let[@opaque] rec rec_c1_1arg () = i64_1
  and[@opaque] rec_c2_1arg () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_2
  and[@opaque] rec_c3_1arg () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    300
  and[@opaque] rec_c1_2arg () () =
    let (_ : int64#) = Sys.opaque_identity i64_3 in
    rec_c1_1arg ()
  and[@opaque] rec_c2_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    let (_ : int64#) = Sys.opaque_identity i64_4 in
    rec_c2_1arg ()
  and[@opaque] rec_c3_2arg () () =
    let (_i : int) = Sys.opaque_identity (String.length x) in
    rec_c3_1arg ()
  in
  ( c1_1arg,
    c2_1arg,
    c3_1arg,
    c1_2arg,
    c2_2arg,
    c3_2arg,
    rec_c1_1arg,
    rec_c2_1arg,
    rec_c3_1arg,
    rec_c1_2arg,
    rec_c2_2arg,
    rec_c3_2arg )

let[@opaque] check_results small_or_large
    (i64_1 : int64#) (i64_2 : int64#) (i64_3 : int64#)
    (i64_4 : int64#) (x : string)
    ( c1_1arg,
      c2_1arg,
      c3_1arg,
      c1_2arg,
      c2_2arg,
      c3_2arg,
      rec_c1_1arg,
      rec_c2_1arg,
      rec_c3_1arg,
      rec_c1_2arg,
      rec_c2_2arg,
      rec_c3_2arg ) =
  let check name b =
    if not b then failwith (small_or_large ^ ": " ^ name)
  in
  check "c1_1" (Int64_u.equal (c1_1arg ()) i64_1);
  check "c2_1" (Int64_u.equal (c2_1arg ()) i64_2);
  check "c3_1" (Int.equal (c3_1arg ()) 100);
  check "c1_2" (Int64_u.equal (c1_2arg () ()) i64_3);
  check "c2_2" (Int64_u.equal (c2_2arg () ()) i64_4);
  check "c3_2" (Int.equal (c3_2arg () ()) 200);
  check "rec_c1_1" (Int64_u.equal (rec_c1_1arg ()) i64_1);
  check "rec_c2_1" (Int64_u.equal (rec_c2_1arg ()) i64_2);
  check "rec_c3_1" (Int.equal (rec_c3_1arg ()) 300);
  check "rec_c1_2" (Int64_u.equal (rec_c1_2arg () ()) i64_1);
  check "rec_c2_2" (Int64_u.equal (rec_c2_2arg () ()) i64_2);
  check "rec_c3_2" (Int.equal (rec_c3_2arg () ()) 300)

let check_tag_and_size v1 v2 =
  let v1 = Obj.repr v1 in
  let v2 = Obj.repr v2 in
  assert (Obj.tag v1 = Obj.tag v2);
  assert (Obj.size v1 = Obj.size v2)

let check_one_small_closures () =
  let i64_1 = rand_near_minor_heap () in
  let i64_2 = rand_near_minor_heap () in
  let i64_3 = rand_near_minor_heap () in
  let i64_4 = rand_near_minor_heap () in
  let x = rand_string () in
  let ( c1_1arg_original,
        c2_1arg_original,
        c3_1arg_original,
        c1_2arg_original,
        c2_2arg_original,
        c3_2arg_original,
        rec_c1_1arg_original,
        rec_c2_1arg_original,
        rec_c3_1arg_original,
        rec_c1_2arg_original,
        rec_c2_2arg_original,
        rec_c3_2arg_original ) =
    make_small_closures i64_1 i64_2 i64_3 i64_4 x
  in
  let dup (type a) (x : a) : a = Obj.(obj (dup (repr x))) in
  let c1_1arg = dup c1_1arg_original in
  let c2_1arg = dup c2_1arg_original in
  let c3_1arg = dup c3_1arg_original in
  let c1_2arg = dup c1_2arg_original in
  let c2_2arg = dup c2_2arg_original in
  let c3_2arg = dup c3_2arg_original in
  let rec_c1_1arg = dup rec_c1_1arg_original in
  let rec_c2_1arg = dup rec_c2_1arg_original in
  let rec_c3_1arg = dup rec_c3_1arg_original in
  let rec_c1_2arg = dup rec_c1_2arg_original in
  let rec_c2_2arg = dup rec_c2_2arg_original in
  let rec_c3_2arg = dup rec_c3_2arg_original in
  Gc.compact ();
  check_tag_and_size c1_1arg c1_1arg_original;
  check_tag_and_size c2_1arg c2_1arg_original;
  check_tag_and_size c3_1arg c3_1arg_original;
  check_tag_and_size c1_2arg c1_2arg_original;
  check_tag_and_size c2_2arg c2_2arg_original;
  check_tag_and_size c3_2arg c3_2arg_original;
  check_tag_and_size rec_c1_1arg rec_c1_1arg_original;
  check_tag_and_size rec_c2_1arg rec_c2_1arg_original;
  check_tag_and_size rec_c3_1arg rec_c3_1arg_original;
  check_tag_and_size rec_c1_2arg rec_c1_2arg_original;
  check_tag_and_size rec_c2_2arg rec_c2_2arg_original;
  check_tag_and_size rec_c3_2arg rec_c3_2arg_original;
  check_results "small" i64_1 i64_2 i64_3 i64_4 x
    ( c1_1arg,
      c2_1arg,
      c3_1arg,
      c1_2arg,
      c2_2arg,
      c3_2arg,
      rec_c1_1arg,
      rec_c2_1arg,
      rec_c3_1arg,
      rec_c1_2arg,
      rec_c2_2arg,
      rec_c3_2arg )

(* Same as above, but with extra environment slots to make sure the
   closures get allocated on the major heap when duplicated.
   We assume Max_young_wosize = 256.
*)
let[@opaque] make_large_closures (i64_1 : int64#) (i64_2 : int64#)
    (i64_3 : int64#) (i64_4 : int64#)
    (padding_i64_0 : int64#)
    (padding_i64_1 : int64#)
    (padding_i64_2 : int64#)
    (padding_i64_3 : int64#)
    (padding_i64_4 : int64#)
    (padding_i64_5 : int64#)
    (padding_i64_6 : int64#)
    (padding_i64_7 : int64#)
    (padding_i64_8 : int64#)
    (padding_i64_9 : int64#)
    (padding_i64_10 : int64#)
    (padding_i64_11 : int64#)
    (padding_i64_12 : int64#)
    (padding_i64_13 : int64#)
    (padding_i64_14 : int64#)
    (padding_i64_15 : int64#)
    (padding_i64_16 : int64#)
    (padding_i64_17 : int64#)
    (padding_i64_18 : int64#)
    (padding_i64_19 : int64#)
    (padding_i64_20 : int64#)
    (padding_i64_21 : int64#)
    (padding_i64_22 : int64#)
    (padding_i64_23 : int64#)
    (padding_i64_24 : int64#)
    (padding_i64_25 : int64#)
    (padding_i64_26 : int64#)
    (padding_i64_27 : int64#)
    (padding_i64_28 : int64#)
    (padding_i64_29 : int64#)
    (padding_i64_30 : int64#)
    (padding_i64_31 : int64#)
    (padding_i64_32 : int64#)
    (padding_i64_33 : int64#)
    (padding_i64_34 : int64#)
    (padding_i64_35 : int64#)
    (padding_i64_36 : int64#)
    (padding_i64_37 : int64#)
    (padding_i64_38 : int64#)
    (padding_i64_39 : int64#)
    (padding_i64_40 : int64#)
    (padding_i64_41 : int64#)
    (padding_i64_42 : int64#)
    (padding_i64_43 : int64#)
    (padding_i64_44 : int64#)
    (padding_i64_45 : int64#)
    (padding_i64_46 : int64#)
    (padding_i64_47 : int64#)
    (padding_i64_48 : int64#)
    (padding_i64_49 : int64#)
    (padding_i64_50 : int64#)
    (padding_i64_51 : int64#)
    (padding_i64_52 : int64#)
    (padding_i64_53 : int64#)
    (padding_i64_54 : int64#)
    (padding_i64_55 : int64#)
    (padding_i64_56 : int64#)
    (padding_i64_57 : int64#)
    (padding_i64_58 : int64#)
    (padding_i64_59 : int64#)
    (padding_i64_60 : int64#)
    (padding_i64_61 : int64#)
    (padding_i64_62 : int64#)
    (padding_i64_63 : int64#)
    (padding_i64_64 : int64#)
    (padding_i64_65 : int64#)
    (padding_i64_66 : int64#)
    (padding_i64_67 : int64#)
    (padding_i64_68 : int64#)
    (padding_i64_69 : int64#)
    (padding_i64_70 : int64#)
    (padding_i64_71 : int64#)
    (padding_i64_72 : int64#)
    (padding_i64_73 : int64#)
    (padding_i64_74 : int64#)
    (padding_i64_75 : int64#)
    (padding_i64_76 : int64#)
    (padding_i64_77 : int64#)
    (padding_i64_78 : int64#)
    (padding_i64_79 : int64#)
    (padding_i64_80 : int64#)
    (padding_i64_81 : int64#)
    (padding_i64_82 : int64#)
    (padding_i64_83 : int64#)
    (padding_i64_84 : int64#)
    (padding_i64_85 : int64#)
    (padding_i64_86 : int64#)
    (padding_i64_87 : int64#)
    (padding_i64_88 : int64#)
    (padding_i64_89 : int64#)
    (padding_i64_90 : int64#)
    (padding_i64_91 : int64#)
    (padding_i64_92 : int64#)
    (padding_i64_93 : int64#)
    (padding_i64_94 : int64#)
    (padding_i64_95 : int64#)
    (padding_i64_96 : int64#)
    (padding_i64_97 : int64#)
    (padding_i64_98 : int64#)
    (padding_i64_99 : int64#)
    (padding_i64_100 : int64#)
    (padding_i64_101 : int64#)
    (padding_i64_102 : int64#)
    (padding_i64_103 : int64#)
    (padding_i64_104 : int64#)
    (padding_i64_105 : int64#)
    (padding_i64_106 : int64#)
    (padding_i64_107 : int64#)
    (padding_i64_108 : int64#)
    (padding_i64_109 : int64#)
    (padding_i64_110 : int64#)
    (padding_i64_111 : int64#)
    (padding_i64_112 : int64#)
    (padding_i64_113 : int64#)
    (padding_i64_114 : int64#)
    (padding_i64_115 : int64#)
    (padding_i64_116 : int64#)
    (padding_i64_117 : int64#)
    (padding_i64_118 : int64#)
    (padding_i64_119 : int64#)
    (padding_i64_120 : int64#)
    (padding_i64_121 : int64#)
    (padding_i64_122 : int64#)
    (padding_i64_123 : int64#)
    (padding_i64_124 : int64#)
    (padding_i64_125 : int64#)
    (padding_i64_126 : int64#)
    (padding_i64_127 : int64#)
    (padding_i64_128 : int64#)
    (padding_i64_129 : int64#)
    (padding_i64_130 : int64#)
    (padding_i64_131 : int64#)
    (padding_i64_132 : int64#)
    (padding_i64_133 : int64#)
    (padding_i64_134 : int64#)
    (padding_i64_135 : int64#)
    (padding_i64_136 : int64#)
    (padding_i64_137 : int64#)
    (padding_i64_138 : int64#)
    (padding_i64_139 : int64#)
    (padding_i64_140 : int64#)
    (padding_i64_141 : int64#)
    (padding_i64_142 : int64#)
    (padding_i64_143 : int64#)
    (padding_i64_144 : int64#)
    (padding_i64_145 : int64#)
    (padding_i64_146 : int64#)
    (padding_i64_147 : int64#)
    (padding_i64_148 : int64#)
    (padding_i64_149 : int64#)
    (padding_i64_150 : int64#)
    (padding_i64_151 : int64#)
    (padding_i64_152 : int64#)
    (padding_i64_153 : int64#)
    (padding_i64_154 : int64#)
    (padding_i64_155 : int64#)
    (padding_i64_156 : int64#)
    (padding_i64_157 : int64#)
    (padding_i64_158 : int64#)
    (padding_i64_159 : int64#)
    (padding_i64_160 : int64#)
    (padding_i64_161 : int64#)
    (padding_i64_162 : int64#)
    (padding_i64_163 : int64#)
    (padding_i64_164 : int64#)
    (padding_i64_165 : int64#)
    (padding_i64_166 : int64#)
    (padding_i64_167 : int64#)
    (padding_i64_168 : int64#)
    (padding_i64_169 : int64#)
    (padding_i64_170 : int64#)
    (padding_i64_171 : int64#)
    (padding_i64_172 : int64#)
    (padding_i64_173 : int64#)
    (padding_i64_174 : int64#)
    (padding_i64_175 : int64#)
    (padding_i64_176 : int64#)
    (padding_i64_177 : int64#)
    (padding_i64_178 : int64#)
    (padding_i64_179 : int64#)
    (padding_i64_180 : int64#)
    (padding_i64_181 : int64#)
    (padding_i64_182 : int64#)
    (padding_i64_183 : int64#)
    (padding_i64_184 : int64#)
    (padding_i64_185 : int64#)
    (padding_i64_186 : int64#)
    (padding_i64_187 : int64#)
    (padding_i64_188 : int64#)
    (padding_i64_189 : int64#)
    (padding_i64_190 : int64#)
    (padding_i64_191 : int64#)
    (padding_i64_192 : int64#)
    (padding_i64_193 : int64#)
    (padding_i64_194 : int64#)
    (padding_i64_195 : int64#)
    (padding_i64_196 : int64#)
    (padding_i64_197 : int64#)
    (padding_i64_198 : int64#)
    (padding_i64_199 : int64#)
    (padding_i64_200 : int64#)
    (padding_i64_201 : int64#)
    (padding_i64_202 : int64#)
    (padding_i64_203 : int64#)
    (padding_i64_204 : int64#)
    (padding_i64_205 : int64#)
    (padding_i64_206 : int64#)
    (padding_i64_207 : int64#)
    (padding_i64_208 : int64#)
    (padding_i64_209 : int64#)
    (padding_i64_210 : int64#)
    (padding_i64_211 : int64#)
    (padding_i64_212 : int64#)
    (padding_i64_213 : int64#)
    (padding_i64_214 : int64#)
    (padding_i64_215 : int64#)
    (padding_i64_216 : int64#)
    (padding_i64_217 : int64#)
    (padding_i64_218 : int64#)
    (padding_i64_219 : int64#)
    (padding_i64_220 : int64#)
    (padding_i64_221 : int64#)
    (padding_i64_222 : int64#)
    (padding_i64_223 : int64#)
    (padding_i64_224 : int64#)
    (padding_i64_225 : int64#)
    (padding_i64_226 : int64#)
    (padding_i64_227 : int64#)
    (padding_i64_228 : int64#)
    (padding_i64_229 : int64#)
    (padding_i64_230 : int64#)
    (padding_i64_231 : int64#)
    (padding_i64_232 : int64#)
    (padding_i64_233 : int64#)
    (padding_i64_234 : int64#)
    (padding_i64_235 : int64#)
    (padding_i64_236 : int64#)
    (padding_i64_237 : int64#)
    (padding_i64_238 : int64#)
    (padding_i64_239 : int64#)
    (padding_i64_240 : int64#)
    (padding_i64_241 : int64#)
    (padding_i64_242 : int64#)
    (padding_i64_243 : int64#)
    (padding_i64_244 : int64#)
    (padding_i64_245 : int64#)
    (padding_i64_246 : int64#)
    (padding_i64_247 : int64#)
    (padding_i64_248 : int64#)
    (padding_i64_249 : int64#)
    (padding_i64_250 : int64#)
    (padding_i64_251 : int64#)
    (padding_i64_252 : int64#)
    (padding_i64_253 : int64#)
    (padding_i64_254 : int64#)
    (padding_i64_255 : int64#)
    (padding_i64_256 : int64#)
    (padding_i64_257 : int64#)
    (padding_i64_258 : int64#)
    (padding_i64_259 : int64#)
    (padding_i64_260 : int64#)
    (padding_i64_261 : int64#)
    (padding_i64_262 : int64#)
    (padding_i64_263 : int64#)
    (padding_i64_264 : int64#)
    (padding_i64_265 : int64#)
    (padding_i64_266 : int64#)
    (padding_i64_267 : int64#)
    (padding_i64_268 : int64#)
    (padding_i64_269 : int64#)
    (padding_i64_270 : int64#)
    (padding_0 : string)
    (padding_1 : string)
    (padding_2 : string)
    (padding_3 : string)
    (padding_4 : string)
    (padding_5 : string)
    (padding_6 : string)
    (padding_7 : string)
    (padding_8 : string)
    (padding_9 : string)
    (padding_10 : string)
    (padding_11 : string)
    (padding_12 : string)
    (padding_13 : string)
    (padding_14 : string)
    (padding_15 : string)
    (padding_16 : string)
    (padding_17 : string)
    (padding_18 : string)
    (padding_19 : string)
    (padding_20 : string)
    (padding_21 : string)
    (padding_22 : string)
    (padding_23 : string)
    (padding_24 : string)
    (padding_25 : string)
    (padding_26 : string)
    (padding_27 : string)
    (padding_28 : string)
    (padding_29 : string)
    (padding_30 : string)
    (padding_31 : string)
    (padding_32 : string)
    (padding_33 : string)
    (padding_34 : string)
    (padding_35 : string)
    (padding_36 : string)
    (padding_37 : string)
    (padding_38 : string)
    (padding_39 : string)
    (padding_40 : string)
    (padding_41 : string)
    (padding_42 : string)
    (padding_43 : string)
    (padding_44 : string)
    (padding_45 : string)
    (padding_46 : string)
    (padding_47 : string)
    (padding_48 : string)
    (padding_49 : string)
    (padding_50 : string)
    (padding_51 : string)
    (padding_52 : string)
    (padding_53 : string)
    (padding_54 : string)
    (padding_55 : string)
    (padding_56 : string)
    (padding_57 : string)
    (padding_58 : string)
    (padding_59 : string)
    (padding_60 : string)
    (padding_61 : string)
    (padding_62 : string)
    (padding_63 : string)
    (padding_64 : string)
    (padding_65 : string)
    (padding_66 : string)
    (padding_67 : string)
    (padding_68 : string)
    (padding_69 : string)
    (padding_70 : string)
    (padding_71 : string)
    (padding_72 : string)
    (padding_73 : string)
    (padding_74 : string)
    (padding_75 : string)
    (padding_76 : string)
    (padding_77 : string)
    (padding_78 : string)
    (padding_79 : string)
    (padding_80 : string)
    (padding_81 : string)
    (padding_82 : string)
    (padding_83 : string)
    (padding_84 : string)
    (padding_85 : string)
    (padding_86 : string)
    (padding_87 : string)
    (padding_88 : string)
    (padding_89 : string)
    (padding_90 : string)
    (padding_91 : string)
    (padding_92 : string)
    (padding_93 : string)
    (padding_94 : string)
    (padding_95 : string)
    (padding_96 : string)
    (padding_97 : string)
    (padding_98 : string)
    (padding_99 : string)
    (padding_100 : string)
    (padding_101 : string)
    (padding_102 : string)
    (padding_103 : string)
    (padding_104 : string)
    (padding_105 : string)
    (padding_106 : string)
    (padding_107 : string)
    (padding_108 : string)
    (padding_109 : string)
    (padding_110 : string)
    (padding_111 : string)
    (padding_112 : string)
    (padding_113 : string)
    (padding_114 : string)
    (padding_115 : string)
    (padding_116 : string)
    (padding_117 : string)
    (padding_118 : string)
    (padding_119 : string)
    (padding_120 : string)
    (padding_121 : string)
    (padding_122 : string)
    (padding_123 : string)
    (padding_124 : string)
    (padding_125 : string)
    (padding_126 : string)
    (padding_127 : string)
    (padding_128 : string)
    (padding_129 : string)
    (padding_130 : string)
    (padding_131 : string)
    (padding_132 : string)
    (padding_133 : string)
    (padding_134 : string)
    (padding_135 : string)
    (padding_136 : string)
    (padding_137 : string)
    (padding_138 : string)
    (padding_139 : string)
    (padding_140 : string)
    (padding_141 : string)
    (padding_142 : string)
    (padding_143 : string)
    (padding_144 : string)
    (padding_145 : string)
    (padding_146 : string)
    (padding_147 : string)
    (padding_148 : string)
    (padding_149 : string)
    (padding_150 : string)
    (padding_151 : string)
    (padding_152 : string)
    (padding_153 : string)
    (padding_154 : string)
    (padding_155 : string)
    (padding_156 : string)
    (padding_157 : string)
    (padding_158 : string)
    (padding_159 : string)
    (padding_160 : string)
    (padding_161 : string)
    (padding_162 : string)
    (padding_163 : string)
    (padding_164 : string)
    (padding_165 : string)
    (padding_166 : string)
    (padding_167 : string)
    (padding_168 : string)
    (padding_169 : string)
    (padding_170 : string)
    (padding_171 : string)
    (padding_172 : string)
    (padding_173 : string)
    (padding_174 : string)
    (padding_175 : string)
    (padding_176 : string)
    (padding_177 : string)
    (padding_178 : string)
    (padding_179 : string)
    (padding_180 : string)
    (padding_181 : string)
    (padding_182 : string)
    (padding_183 : string)
    (padding_184 : string)
    (padding_185 : string)
    (padding_186 : string)
    (padding_187 : string)
    (padding_188 : string)
    (padding_189 : string)
    (padding_190 : string)
    (padding_191 : string)
    (padding_192 : string)
    (padding_193 : string)
    (padding_194 : string)
    (padding_195 : string)
    (padding_196 : string)
    (padding_197 : string)
    (padding_198 : string)
    (padding_199 : string)
    (padding_200 : string)
    (padding_201 : string)
    (padding_202 : string)
    (padding_203 : string)
    (padding_204 : string)
    (padding_205 : string)
    (padding_206 : string)
    (padding_207 : string)
    (padding_208 : string)
    (padding_209 : string)
    (padding_210 : string)
    (padding_211 : string)
    (padding_212 : string)
    (padding_213 : string)
    (padding_214 : string)
    (padding_215 : string)
    (padding_216 : string)
    (padding_217 : string)
    (padding_218 : string)
    (padding_219 : string)
    (padding_220 : string)
    (padding_221 : string)
    (padding_222 : string)
    (padding_223 : string)
    (padding_224 : string)
    (padding_225 : string)
    (padding_226 : string)
    (padding_227 : string)
    (padding_228 : string)
    (padding_229 : string)
    (padding_230 : string)
    (padding_231 : string)
    (padding_232 : string)
    (padding_233 : string)
    (padding_234 : string)
    (padding_235 : string)
    (padding_236 : string)
    (padding_237 : string)
    (padding_238 : string)
    (padding_239 : string)
    (padding_240 : string)
    (padding_241 : string)
    (padding_242 : string)
    (padding_243 : string)
    (padding_244 : string)
    (padding_245 : string)
    (padding_246 : string)
    (padding_247 : string)
    (padding_248 : string)
    (padding_249 : string)
    (padding_250 : string)
    (padding_251 : string)
    (padding_252 : string)
    (padding_253 : string)
    (padding_254 : string)
    (padding_255 : string)
    (padding_256 : string)
    (padding_257 : string)
    (padding_258 : string)
    (padding_259 : string)
    (padding_260 : string)
    (padding_261 : string)
    (padding_262 : string)
    (padding_263 : string)
    (padding_264 : string)
    (padding_265 : string)
    (padding_266 : string)
    (padding_267 : string)
    (padding_268 : string)
    (padding_269 : string)
    (padding_270 : string)
    (x : string) =
  (* Two-word function slot (i.e. one argument) cases *)
  let[@opaque] c1_1arg () =
    (* Only an unboxed environment *)
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    i64_1
  in
  let[@opaque] c2_1arg () =
    (* An unboxed environment plus a scannable environment *)
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_2
  in
  let[@opaque] c3_1arg () =
    (* Only a scannable environment *)
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_1 in
    let (_ : string) = Sys.opaque_identity padding_2 in
    let (_ : string) = Sys.opaque_identity padding_3 in
    let (_ : string) = Sys.opaque_identity padding_4 in
    let (_ : string) = Sys.opaque_identity padding_5 in
    let (_ : string) = Sys.opaque_identity padding_6 in
    let (_ : string) = Sys.opaque_identity padding_7 in
    let (_ : string) = Sys.opaque_identity padding_8 in
    let (_ : string) = Sys.opaque_identity padding_9 in
    let (_ : string) = Sys.opaque_identity padding_10 in
    let (_ : string) = Sys.opaque_identity padding_11 in
    let (_ : string) = Sys.opaque_identity padding_12 in
    let (_ : string) = Sys.opaque_identity padding_13 in
    let (_ : string) = Sys.opaque_identity padding_14 in
    let (_ : string) = Sys.opaque_identity padding_15 in
    let (_ : string) = Sys.opaque_identity padding_16 in
    let (_ : string) = Sys.opaque_identity padding_17 in
    let (_ : string) = Sys.opaque_identity padding_18 in
    let (_ : string) = Sys.opaque_identity padding_19 in
    let (_ : string) = Sys.opaque_identity padding_20 in
    let (_ : string) = Sys.opaque_identity padding_21 in
    let (_ : string) = Sys.opaque_identity padding_22 in
    let (_ : string) = Sys.opaque_identity padding_23 in
    let (_ : string) = Sys.opaque_identity padding_24 in
    let (_ : string) = Sys.opaque_identity padding_25 in
    let (_ : string) = Sys.opaque_identity padding_26 in
    let (_ : string) = Sys.opaque_identity padding_27 in
    let (_ : string) = Sys.opaque_identity padding_28 in
    let (_ : string) = Sys.opaque_identity padding_29 in
    let (_ : string) = Sys.opaque_identity padding_30 in
    let (_ : string) = Sys.opaque_identity padding_31 in
    let (_ : string) = Sys.opaque_identity padding_32 in
    let (_ : string) = Sys.opaque_identity padding_33 in
    let (_ : string) = Sys.opaque_identity padding_34 in
    let (_ : string) = Sys.opaque_identity padding_35 in
    let (_ : string) = Sys.opaque_identity padding_36 in
    let (_ : string) = Sys.opaque_identity padding_37 in
    let (_ : string) = Sys.opaque_identity padding_38 in
    let (_ : string) = Sys.opaque_identity padding_39 in
    let (_ : string) = Sys.opaque_identity padding_40 in
    let (_ : string) = Sys.opaque_identity padding_41 in
    let (_ : string) = Sys.opaque_identity padding_42 in
    let (_ : string) = Sys.opaque_identity padding_43 in
    let (_ : string) = Sys.opaque_identity padding_44 in
    let (_ : string) = Sys.opaque_identity padding_45 in
    let (_ : string) = Sys.opaque_identity padding_46 in
    let (_ : string) = Sys.opaque_identity padding_47 in
    let (_ : string) = Sys.opaque_identity padding_48 in
    let (_ : string) = Sys.opaque_identity padding_49 in
    let (_ : string) = Sys.opaque_identity padding_50 in
    let (_ : string) = Sys.opaque_identity padding_51 in
    let (_ : string) = Sys.opaque_identity padding_52 in
    let (_ : string) = Sys.opaque_identity padding_53 in
    let (_ : string) = Sys.opaque_identity padding_54 in
    let (_ : string) = Sys.opaque_identity padding_55 in
    let (_ : string) = Sys.opaque_identity padding_56 in
    let (_ : string) = Sys.opaque_identity padding_57 in
    let (_ : string) = Sys.opaque_identity padding_58 in
    let (_ : string) = Sys.opaque_identity padding_59 in
    let (_ : string) = Sys.opaque_identity padding_60 in
    let (_ : string) = Sys.opaque_identity padding_61 in
    let (_ : string) = Sys.opaque_identity padding_62 in
    let (_ : string) = Sys.opaque_identity padding_63 in
    let (_ : string) = Sys.opaque_identity padding_64 in
    let (_ : string) = Sys.opaque_identity padding_65 in
    let (_ : string) = Sys.opaque_identity padding_66 in
    let (_ : string) = Sys.opaque_identity padding_67 in
    let (_ : string) = Sys.opaque_identity padding_68 in
    let (_ : string) = Sys.opaque_identity padding_69 in
    let (_ : string) = Sys.opaque_identity padding_70 in
    let (_ : string) = Sys.opaque_identity padding_71 in
    let (_ : string) = Sys.opaque_identity padding_72 in
    let (_ : string) = Sys.opaque_identity padding_73 in
    let (_ : string) = Sys.opaque_identity padding_74 in
    let (_ : string) = Sys.opaque_identity padding_75 in
    let (_ : string) = Sys.opaque_identity padding_76 in
    let (_ : string) = Sys.opaque_identity padding_77 in
    let (_ : string) = Sys.opaque_identity padding_78 in
    let (_ : string) = Sys.opaque_identity padding_79 in
    let (_ : string) = Sys.opaque_identity padding_80 in
    let (_ : string) = Sys.opaque_identity padding_81 in
    let (_ : string) = Sys.opaque_identity padding_82 in
    let (_ : string) = Sys.opaque_identity padding_83 in
    let (_ : string) = Sys.opaque_identity padding_84 in
    let (_ : string) = Sys.opaque_identity padding_85 in
    let (_ : string) = Sys.opaque_identity padding_86 in
    let (_ : string) = Sys.opaque_identity padding_87 in
    let (_ : string) = Sys.opaque_identity padding_88 in
    let (_ : string) = Sys.opaque_identity padding_89 in
    let (_ : string) = Sys.opaque_identity padding_90 in
    let (_ : string) = Sys.opaque_identity padding_91 in
    let (_ : string) = Sys.opaque_identity padding_92 in
    let (_ : string) = Sys.opaque_identity padding_93 in
    let (_ : string) = Sys.opaque_identity padding_94 in
    let (_ : string) = Sys.opaque_identity padding_95 in
    let (_ : string) = Sys.opaque_identity padding_96 in
    let (_ : string) = Sys.opaque_identity padding_97 in
    let (_ : string) = Sys.opaque_identity padding_98 in
    let (_ : string) = Sys.opaque_identity padding_99 in
    let (_ : string) = Sys.opaque_identity padding_100 in
    let (_ : string) = Sys.opaque_identity padding_101 in
    let (_ : string) = Sys.opaque_identity padding_102 in
    let (_ : string) = Sys.opaque_identity padding_103 in
    let (_ : string) = Sys.opaque_identity padding_104 in
    let (_ : string) = Sys.opaque_identity padding_105 in
    let (_ : string) = Sys.opaque_identity padding_106 in
    let (_ : string) = Sys.opaque_identity padding_107 in
    let (_ : string) = Sys.opaque_identity padding_108 in
    let (_ : string) = Sys.opaque_identity padding_109 in
    let (_ : string) = Sys.opaque_identity padding_110 in
    let (_ : string) = Sys.opaque_identity padding_111 in
    let (_ : string) = Sys.opaque_identity padding_112 in
    let (_ : string) = Sys.opaque_identity padding_113 in
    let (_ : string) = Sys.opaque_identity padding_114 in
    let (_ : string) = Sys.opaque_identity padding_115 in
    let (_ : string) = Sys.opaque_identity padding_116 in
    let (_ : string) = Sys.opaque_identity padding_117 in
    let (_ : string) = Sys.opaque_identity padding_118 in
    let (_ : string) = Sys.opaque_identity padding_119 in
    let (_ : string) = Sys.opaque_identity padding_120 in
    let (_ : string) = Sys.opaque_identity padding_121 in
    let (_ : string) = Sys.opaque_identity padding_122 in
    let (_ : string) = Sys.opaque_identity padding_123 in
    let (_ : string) = Sys.opaque_identity padding_124 in
    let (_ : string) = Sys.opaque_identity padding_125 in
    let (_ : string) = Sys.opaque_identity padding_126 in
    let (_ : string) = Sys.opaque_identity padding_127 in
    let (_ : string) = Sys.opaque_identity padding_128 in
    let (_ : string) = Sys.opaque_identity padding_129 in
    let (_ : string) = Sys.opaque_identity padding_130 in
    let (_ : string) = Sys.opaque_identity padding_131 in
    let (_ : string) = Sys.opaque_identity padding_132 in
    let (_ : string) = Sys.opaque_identity padding_133 in
    let (_ : string) = Sys.opaque_identity padding_134 in
    let (_ : string) = Sys.opaque_identity padding_135 in
    let (_ : string) = Sys.opaque_identity padding_136 in
    let (_ : string) = Sys.opaque_identity padding_137 in
    let (_ : string) = Sys.opaque_identity padding_138 in
    let (_ : string) = Sys.opaque_identity padding_139 in
    let (_ : string) = Sys.opaque_identity padding_140 in
    let (_ : string) = Sys.opaque_identity padding_141 in
    let (_ : string) = Sys.opaque_identity padding_142 in
    let (_ : string) = Sys.opaque_identity padding_143 in
    let (_ : string) = Sys.opaque_identity padding_144 in
    let (_ : string) = Sys.opaque_identity padding_145 in
    let (_ : string) = Sys.opaque_identity padding_146 in
    let (_ : string) = Sys.opaque_identity padding_147 in
    let (_ : string) = Sys.opaque_identity padding_148 in
    let (_ : string) = Sys.opaque_identity padding_149 in
    let (_ : string) = Sys.opaque_identity padding_150 in
    let (_ : string) = Sys.opaque_identity padding_151 in
    let (_ : string) = Sys.opaque_identity padding_152 in
    let (_ : string) = Sys.opaque_identity padding_153 in
    let (_ : string) = Sys.opaque_identity padding_154 in
    let (_ : string) = Sys.opaque_identity padding_155 in
    let (_ : string) = Sys.opaque_identity padding_156 in
    let (_ : string) = Sys.opaque_identity padding_157 in
    let (_ : string) = Sys.opaque_identity padding_158 in
    let (_ : string) = Sys.opaque_identity padding_159 in
    let (_ : string) = Sys.opaque_identity padding_160 in
    let (_ : string) = Sys.opaque_identity padding_161 in
    let (_ : string) = Sys.opaque_identity padding_162 in
    let (_ : string) = Sys.opaque_identity padding_163 in
    let (_ : string) = Sys.opaque_identity padding_164 in
    let (_ : string) = Sys.opaque_identity padding_165 in
    let (_ : string) = Sys.opaque_identity padding_166 in
    let (_ : string) = Sys.opaque_identity padding_167 in
    let (_ : string) = Sys.opaque_identity padding_168 in
    let (_ : string) = Sys.opaque_identity padding_169 in
    let (_ : string) = Sys.opaque_identity padding_170 in
    let (_ : string) = Sys.opaque_identity padding_171 in
    let (_ : string) = Sys.opaque_identity padding_172 in
    let (_ : string) = Sys.opaque_identity padding_173 in
    let (_ : string) = Sys.opaque_identity padding_174 in
    let (_ : string) = Sys.opaque_identity padding_175 in
    let (_ : string) = Sys.opaque_identity padding_176 in
    let (_ : string) = Sys.opaque_identity padding_177 in
    let (_ : string) = Sys.opaque_identity padding_178 in
    let (_ : string) = Sys.opaque_identity padding_179 in
    let (_ : string) = Sys.opaque_identity padding_180 in
    let (_ : string) = Sys.opaque_identity padding_181 in
    let (_ : string) = Sys.opaque_identity padding_182 in
    let (_ : string) = Sys.opaque_identity padding_183 in
    let (_ : string) = Sys.opaque_identity padding_184 in
    let (_ : string) = Sys.opaque_identity padding_185 in
    let (_ : string) = Sys.opaque_identity padding_186 in
    let (_ : string) = Sys.opaque_identity padding_187 in
    let (_ : string) = Sys.opaque_identity padding_188 in
    let (_ : string) = Sys.opaque_identity padding_189 in
    let (_ : string) = Sys.opaque_identity padding_190 in
    let (_ : string) = Sys.opaque_identity padding_191 in
    let (_ : string) = Sys.opaque_identity padding_192 in
    let (_ : string) = Sys.opaque_identity padding_193 in
    let (_ : string) = Sys.opaque_identity padding_194 in
    let (_ : string) = Sys.opaque_identity padding_195 in
    let (_ : string) = Sys.opaque_identity padding_196 in
    let (_ : string) = Sys.opaque_identity padding_197 in
    let (_ : string) = Sys.opaque_identity padding_198 in
    let (_ : string) = Sys.opaque_identity padding_199 in
    let (_ : string) = Sys.opaque_identity padding_200 in
    let (_ : string) = Sys.opaque_identity padding_201 in
    let (_ : string) = Sys.opaque_identity padding_202 in
    let (_ : string) = Sys.opaque_identity padding_203 in
    let (_ : string) = Sys.opaque_identity padding_204 in
    let (_ : string) = Sys.opaque_identity padding_205 in
    let (_ : string) = Sys.opaque_identity padding_206 in
    let (_ : string) = Sys.opaque_identity padding_207 in
    let (_ : string) = Sys.opaque_identity padding_208 in
    let (_ : string) = Sys.opaque_identity padding_209 in
    let (_ : string) = Sys.opaque_identity padding_210 in
    let (_ : string) = Sys.opaque_identity padding_211 in
    let (_ : string) = Sys.opaque_identity padding_212 in
    let (_ : string) = Sys.opaque_identity padding_213 in
    let (_ : string) = Sys.opaque_identity padding_214 in
    let (_ : string) = Sys.opaque_identity padding_215 in
    let (_ : string) = Sys.opaque_identity padding_216 in
    let (_ : string) = Sys.opaque_identity padding_217 in
    let (_ : string) = Sys.opaque_identity padding_218 in
    let (_ : string) = Sys.opaque_identity padding_219 in
    let (_ : string) = Sys.opaque_identity padding_220 in
    let (_ : string) = Sys.opaque_identity padding_221 in
    let (_ : string) = Sys.opaque_identity padding_222 in
    let (_ : string) = Sys.opaque_identity padding_223 in
    let (_ : string) = Sys.opaque_identity padding_224 in
    let (_ : string) = Sys.opaque_identity padding_225 in
    let (_ : string) = Sys.opaque_identity padding_226 in
    let (_ : string) = Sys.opaque_identity padding_227 in
    let (_ : string) = Sys.opaque_identity padding_228 in
    let (_ : string) = Sys.opaque_identity padding_229 in
    let (_ : string) = Sys.opaque_identity padding_230 in
    let (_ : string) = Sys.opaque_identity padding_231 in
    let (_ : string) = Sys.opaque_identity padding_232 in
    let (_ : string) = Sys.opaque_identity padding_233 in
    let (_ : string) = Sys.opaque_identity padding_234 in
    let (_ : string) = Sys.opaque_identity padding_235 in
    let (_ : string) = Sys.opaque_identity padding_236 in
    let (_ : string) = Sys.opaque_identity padding_237 in
    let (_ : string) = Sys.opaque_identity padding_238 in
    let (_ : string) = Sys.opaque_identity padding_239 in
    let (_ : string) = Sys.opaque_identity padding_240 in
    let (_ : string) = Sys.opaque_identity padding_241 in
    let (_ : string) = Sys.opaque_identity padding_242 in
    let (_ : string) = Sys.opaque_identity padding_243 in
    let (_ : string) = Sys.opaque_identity padding_244 in
    let (_ : string) = Sys.opaque_identity padding_245 in
    let (_ : string) = Sys.opaque_identity padding_246 in
    let (_ : string) = Sys.opaque_identity padding_247 in
    let (_ : string) = Sys.opaque_identity padding_248 in
    let (_ : string) = Sys.opaque_identity padding_249 in
    let (_ : string) = Sys.opaque_identity padding_250 in
    let (_ : string) = Sys.opaque_identity padding_251 in
    let (_ : string) = Sys.opaque_identity padding_252 in
    let (_ : string) = Sys.opaque_identity padding_253 in
    let (_ : string) = Sys.opaque_identity padding_254 in
    let (_ : string) = Sys.opaque_identity padding_255 in
    let (_ : string) = Sys.opaque_identity padding_256 in
    let (_ : string) = Sys.opaque_identity padding_257 in
    let (_ : string) = Sys.opaque_identity padding_258 in
    let (_ : string) = Sys.opaque_identity padding_259 in
    let (_ : string) = Sys.opaque_identity padding_260 in
    let (_ : string) = Sys.opaque_identity padding_261 in
    let (_ : string) = Sys.opaque_identity padding_262 in
    let (_ : string) = Sys.opaque_identity padding_263 in
    let (_ : string) = Sys.opaque_identity padding_264 in
    let (_ : string) = Sys.opaque_identity padding_265 in
    let (_ : string) = Sys.opaque_identity padding_266 in
    let (_ : string) = Sys.opaque_identity padding_267 in
    let (_ : string) = Sys.opaque_identity padding_268 in
    let (_ : string) = Sys.opaque_identity padding_269 in
    let (_ : string) = Sys.opaque_identity padding_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    100
  in
  (* Three-word function slot (i.e. more than one argument) cases *)
  let[@opaque] c1_2arg () () =
    (* Only an unboxed environment *)
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    i64_3
  in
  let[@opaque] c2_2arg () () =
    (* An unboxed environment plus a scannable environment *)
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_4
  in
  let[@opaque] c3_2arg () () =
    (* Only a scannable environment *)
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_1 in
    let (_ : string) = Sys.opaque_identity padding_2 in
    let (_ : string) = Sys.opaque_identity padding_3 in
    let (_ : string) = Sys.opaque_identity padding_4 in
    let (_ : string) = Sys.opaque_identity padding_5 in
    let (_ : string) = Sys.opaque_identity padding_6 in
    let (_ : string) = Sys.opaque_identity padding_7 in
    let (_ : string) = Sys.opaque_identity padding_8 in
    let (_ : string) = Sys.opaque_identity padding_9 in
    let (_ : string) = Sys.opaque_identity padding_10 in
    let (_ : string) = Sys.opaque_identity padding_11 in
    let (_ : string) = Sys.opaque_identity padding_12 in
    let (_ : string) = Sys.opaque_identity padding_13 in
    let (_ : string) = Sys.opaque_identity padding_14 in
    let (_ : string) = Sys.opaque_identity padding_15 in
    let (_ : string) = Sys.opaque_identity padding_16 in
    let (_ : string) = Sys.opaque_identity padding_17 in
    let (_ : string) = Sys.opaque_identity padding_18 in
    let (_ : string) = Sys.opaque_identity padding_19 in
    let (_ : string) = Sys.opaque_identity padding_20 in
    let (_ : string) = Sys.opaque_identity padding_21 in
    let (_ : string) = Sys.opaque_identity padding_22 in
    let (_ : string) = Sys.opaque_identity padding_23 in
    let (_ : string) = Sys.opaque_identity padding_24 in
    let (_ : string) = Sys.opaque_identity padding_25 in
    let (_ : string) = Sys.opaque_identity padding_26 in
    let (_ : string) = Sys.opaque_identity padding_27 in
    let (_ : string) = Sys.opaque_identity padding_28 in
    let (_ : string) = Sys.opaque_identity padding_29 in
    let (_ : string) = Sys.opaque_identity padding_30 in
    let (_ : string) = Sys.opaque_identity padding_31 in
    let (_ : string) = Sys.opaque_identity padding_32 in
    let (_ : string) = Sys.opaque_identity padding_33 in
    let (_ : string) = Sys.opaque_identity padding_34 in
    let (_ : string) = Sys.opaque_identity padding_35 in
    let (_ : string) = Sys.opaque_identity padding_36 in
    let (_ : string) = Sys.opaque_identity padding_37 in
    let (_ : string) = Sys.opaque_identity padding_38 in
    let (_ : string) = Sys.opaque_identity padding_39 in
    let (_ : string) = Sys.opaque_identity padding_40 in
    let (_ : string) = Sys.opaque_identity padding_41 in
    let (_ : string) = Sys.opaque_identity padding_42 in
    let (_ : string) = Sys.opaque_identity padding_43 in
    let (_ : string) = Sys.opaque_identity padding_44 in
    let (_ : string) = Sys.opaque_identity padding_45 in
    let (_ : string) = Sys.opaque_identity padding_46 in
    let (_ : string) = Sys.opaque_identity padding_47 in
    let (_ : string) = Sys.opaque_identity padding_48 in
    let (_ : string) = Sys.opaque_identity padding_49 in
    let (_ : string) = Sys.opaque_identity padding_50 in
    let (_ : string) = Sys.opaque_identity padding_51 in
    let (_ : string) = Sys.opaque_identity padding_52 in
    let (_ : string) = Sys.opaque_identity padding_53 in
    let (_ : string) = Sys.opaque_identity padding_54 in
    let (_ : string) = Sys.opaque_identity padding_55 in
    let (_ : string) = Sys.opaque_identity padding_56 in
    let (_ : string) = Sys.opaque_identity padding_57 in
    let (_ : string) = Sys.opaque_identity padding_58 in
    let (_ : string) = Sys.opaque_identity padding_59 in
    let (_ : string) = Sys.opaque_identity padding_60 in
    let (_ : string) = Sys.opaque_identity padding_61 in
    let (_ : string) = Sys.opaque_identity padding_62 in
    let (_ : string) = Sys.opaque_identity padding_63 in
    let (_ : string) = Sys.opaque_identity padding_64 in
    let (_ : string) = Sys.opaque_identity padding_65 in
    let (_ : string) = Sys.opaque_identity padding_66 in
    let (_ : string) = Sys.opaque_identity padding_67 in
    let (_ : string) = Sys.opaque_identity padding_68 in
    let (_ : string) = Sys.opaque_identity padding_69 in
    let (_ : string) = Sys.opaque_identity padding_70 in
    let (_ : string) = Sys.opaque_identity padding_71 in
    let (_ : string) = Sys.opaque_identity padding_72 in
    let (_ : string) = Sys.opaque_identity padding_73 in
    let (_ : string) = Sys.opaque_identity padding_74 in
    let (_ : string) = Sys.opaque_identity padding_75 in
    let (_ : string) = Sys.opaque_identity padding_76 in
    let (_ : string) = Sys.opaque_identity padding_77 in
    let (_ : string) = Sys.opaque_identity padding_78 in
    let (_ : string) = Sys.opaque_identity padding_79 in
    let (_ : string) = Sys.opaque_identity padding_80 in
    let (_ : string) = Sys.opaque_identity padding_81 in
    let (_ : string) = Sys.opaque_identity padding_82 in
    let (_ : string) = Sys.opaque_identity padding_83 in
    let (_ : string) = Sys.opaque_identity padding_84 in
    let (_ : string) = Sys.opaque_identity padding_85 in
    let (_ : string) = Sys.opaque_identity padding_86 in
    let (_ : string) = Sys.opaque_identity padding_87 in
    let (_ : string) = Sys.opaque_identity padding_88 in
    let (_ : string) = Sys.opaque_identity padding_89 in
    let (_ : string) = Sys.opaque_identity padding_90 in
    let (_ : string) = Sys.opaque_identity padding_91 in
    let (_ : string) = Sys.opaque_identity padding_92 in
    let (_ : string) = Sys.opaque_identity padding_93 in
    let (_ : string) = Sys.opaque_identity padding_94 in
    let (_ : string) = Sys.opaque_identity padding_95 in
    let (_ : string) = Sys.opaque_identity padding_96 in
    let (_ : string) = Sys.opaque_identity padding_97 in
    let (_ : string) = Sys.opaque_identity padding_98 in
    let (_ : string) = Sys.opaque_identity padding_99 in
    let (_ : string) = Sys.opaque_identity padding_100 in
    let (_ : string) = Sys.opaque_identity padding_101 in
    let (_ : string) = Sys.opaque_identity padding_102 in
    let (_ : string) = Sys.opaque_identity padding_103 in
    let (_ : string) = Sys.opaque_identity padding_104 in
    let (_ : string) = Sys.opaque_identity padding_105 in
    let (_ : string) = Sys.opaque_identity padding_106 in
    let (_ : string) = Sys.opaque_identity padding_107 in
    let (_ : string) = Sys.opaque_identity padding_108 in
    let (_ : string) = Sys.opaque_identity padding_109 in
    let (_ : string) = Sys.opaque_identity padding_110 in
    let (_ : string) = Sys.opaque_identity padding_111 in
    let (_ : string) = Sys.opaque_identity padding_112 in
    let (_ : string) = Sys.opaque_identity padding_113 in
    let (_ : string) = Sys.opaque_identity padding_114 in
    let (_ : string) = Sys.opaque_identity padding_115 in
    let (_ : string) = Sys.opaque_identity padding_116 in
    let (_ : string) = Sys.opaque_identity padding_117 in
    let (_ : string) = Sys.opaque_identity padding_118 in
    let (_ : string) = Sys.opaque_identity padding_119 in
    let (_ : string) = Sys.opaque_identity padding_120 in
    let (_ : string) = Sys.opaque_identity padding_121 in
    let (_ : string) = Sys.opaque_identity padding_122 in
    let (_ : string) = Sys.opaque_identity padding_123 in
    let (_ : string) = Sys.opaque_identity padding_124 in
    let (_ : string) = Sys.opaque_identity padding_125 in
    let (_ : string) = Sys.opaque_identity padding_126 in
    let (_ : string) = Sys.opaque_identity padding_127 in
    let (_ : string) = Sys.opaque_identity padding_128 in
    let (_ : string) = Sys.opaque_identity padding_129 in
    let (_ : string) = Sys.opaque_identity padding_130 in
    let (_ : string) = Sys.opaque_identity padding_131 in
    let (_ : string) = Sys.opaque_identity padding_132 in
    let (_ : string) = Sys.opaque_identity padding_133 in
    let (_ : string) = Sys.opaque_identity padding_134 in
    let (_ : string) = Sys.opaque_identity padding_135 in
    let (_ : string) = Sys.opaque_identity padding_136 in
    let (_ : string) = Sys.opaque_identity padding_137 in
    let (_ : string) = Sys.opaque_identity padding_138 in
    let (_ : string) = Sys.opaque_identity padding_139 in
    let (_ : string) = Sys.opaque_identity padding_140 in
    let (_ : string) = Sys.opaque_identity padding_141 in
    let (_ : string) = Sys.opaque_identity padding_142 in
    let (_ : string) = Sys.opaque_identity padding_143 in
    let (_ : string) = Sys.opaque_identity padding_144 in
    let (_ : string) = Sys.opaque_identity padding_145 in
    let (_ : string) = Sys.opaque_identity padding_146 in
    let (_ : string) = Sys.opaque_identity padding_147 in
    let (_ : string) = Sys.opaque_identity padding_148 in
    let (_ : string) = Sys.opaque_identity padding_149 in
    let (_ : string) = Sys.opaque_identity padding_150 in
    let (_ : string) = Sys.opaque_identity padding_151 in
    let (_ : string) = Sys.opaque_identity padding_152 in
    let (_ : string) = Sys.opaque_identity padding_153 in
    let (_ : string) = Sys.opaque_identity padding_154 in
    let (_ : string) = Sys.opaque_identity padding_155 in
    let (_ : string) = Sys.opaque_identity padding_156 in
    let (_ : string) = Sys.opaque_identity padding_157 in
    let (_ : string) = Sys.opaque_identity padding_158 in
    let (_ : string) = Sys.opaque_identity padding_159 in
    let (_ : string) = Sys.opaque_identity padding_160 in
    let (_ : string) = Sys.opaque_identity padding_161 in
    let (_ : string) = Sys.opaque_identity padding_162 in
    let (_ : string) = Sys.opaque_identity padding_163 in
    let (_ : string) = Sys.opaque_identity padding_164 in
    let (_ : string) = Sys.opaque_identity padding_165 in
    let (_ : string) = Sys.opaque_identity padding_166 in
    let (_ : string) = Sys.opaque_identity padding_167 in
    let (_ : string) = Sys.opaque_identity padding_168 in
    let (_ : string) = Sys.opaque_identity padding_169 in
    let (_ : string) = Sys.opaque_identity padding_170 in
    let (_ : string) = Sys.opaque_identity padding_171 in
    let (_ : string) = Sys.opaque_identity padding_172 in
    let (_ : string) = Sys.opaque_identity padding_173 in
    let (_ : string) = Sys.opaque_identity padding_174 in
    let (_ : string) = Sys.opaque_identity padding_175 in
    let (_ : string) = Sys.opaque_identity padding_176 in
    let (_ : string) = Sys.opaque_identity padding_177 in
    let (_ : string) = Sys.opaque_identity padding_178 in
    let (_ : string) = Sys.opaque_identity padding_179 in
    let (_ : string) = Sys.opaque_identity padding_180 in
    let (_ : string) = Sys.opaque_identity padding_181 in
    let (_ : string) = Sys.opaque_identity padding_182 in
    let (_ : string) = Sys.opaque_identity padding_183 in
    let (_ : string) = Sys.opaque_identity padding_184 in
    let (_ : string) = Sys.opaque_identity padding_185 in
    let (_ : string) = Sys.opaque_identity padding_186 in
    let (_ : string) = Sys.opaque_identity padding_187 in
    let (_ : string) = Sys.opaque_identity padding_188 in
    let (_ : string) = Sys.opaque_identity padding_189 in
    let (_ : string) = Sys.opaque_identity padding_190 in
    let (_ : string) = Sys.opaque_identity padding_191 in
    let (_ : string) = Sys.opaque_identity padding_192 in
    let (_ : string) = Sys.opaque_identity padding_193 in
    let (_ : string) = Sys.opaque_identity padding_194 in
    let (_ : string) = Sys.opaque_identity padding_195 in
    let (_ : string) = Sys.opaque_identity padding_196 in
    let (_ : string) = Sys.opaque_identity padding_197 in
    let (_ : string) = Sys.opaque_identity padding_198 in
    let (_ : string) = Sys.opaque_identity padding_199 in
    let (_ : string) = Sys.opaque_identity padding_200 in
    let (_ : string) = Sys.opaque_identity padding_201 in
    let (_ : string) = Sys.opaque_identity padding_202 in
    let (_ : string) = Sys.opaque_identity padding_203 in
    let (_ : string) = Sys.opaque_identity padding_204 in
    let (_ : string) = Sys.opaque_identity padding_205 in
    let (_ : string) = Sys.opaque_identity padding_206 in
    let (_ : string) = Sys.opaque_identity padding_207 in
    let (_ : string) = Sys.opaque_identity padding_208 in
    let (_ : string) = Sys.opaque_identity padding_209 in
    let (_ : string) = Sys.opaque_identity padding_210 in
    let (_ : string) = Sys.opaque_identity padding_211 in
    let (_ : string) = Sys.opaque_identity padding_212 in
    let (_ : string) = Sys.opaque_identity padding_213 in
    let (_ : string) = Sys.opaque_identity padding_214 in
    let (_ : string) = Sys.opaque_identity padding_215 in
    let (_ : string) = Sys.opaque_identity padding_216 in
    let (_ : string) = Sys.opaque_identity padding_217 in
    let (_ : string) = Sys.opaque_identity padding_218 in
    let (_ : string) = Sys.opaque_identity padding_219 in
    let (_ : string) = Sys.opaque_identity padding_220 in
    let (_ : string) = Sys.opaque_identity padding_221 in
    let (_ : string) = Sys.opaque_identity padding_222 in
    let (_ : string) = Sys.opaque_identity padding_223 in
    let (_ : string) = Sys.opaque_identity padding_224 in
    let (_ : string) = Sys.opaque_identity padding_225 in
    let (_ : string) = Sys.opaque_identity padding_226 in
    let (_ : string) = Sys.opaque_identity padding_227 in
    let (_ : string) = Sys.opaque_identity padding_228 in
    let (_ : string) = Sys.opaque_identity padding_229 in
    let (_ : string) = Sys.opaque_identity padding_230 in
    let (_ : string) = Sys.opaque_identity padding_231 in
    let (_ : string) = Sys.opaque_identity padding_232 in
    let (_ : string) = Sys.opaque_identity padding_233 in
    let (_ : string) = Sys.opaque_identity padding_234 in
    let (_ : string) = Sys.opaque_identity padding_235 in
    let (_ : string) = Sys.opaque_identity padding_236 in
    let (_ : string) = Sys.opaque_identity padding_237 in
    let (_ : string) = Sys.opaque_identity padding_238 in
    let (_ : string) = Sys.opaque_identity padding_239 in
    let (_ : string) = Sys.opaque_identity padding_240 in
    let (_ : string) = Sys.opaque_identity padding_241 in
    let (_ : string) = Sys.opaque_identity padding_242 in
    let (_ : string) = Sys.opaque_identity padding_243 in
    let (_ : string) = Sys.opaque_identity padding_244 in
    let (_ : string) = Sys.opaque_identity padding_245 in
    let (_ : string) = Sys.opaque_identity padding_246 in
    let (_ : string) = Sys.opaque_identity padding_247 in
    let (_ : string) = Sys.opaque_identity padding_248 in
    let (_ : string) = Sys.opaque_identity padding_249 in
    let (_ : string) = Sys.opaque_identity padding_250 in
    let (_ : string) = Sys.opaque_identity padding_251 in
    let (_ : string) = Sys.opaque_identity padding_252 in
    let (_ : string) = Sys.opaque_identity padding_253 in
    let (_ : string) = Sys.opaque_identity padding_254 in
    let (_ : string) = Sys.opaque_identity padding_255 in
    let (_ : string) = Sys.opaque_identity padding_256 in
    let (_ : string) = Sys.opaque_identity padding_257 in
    let (_ : string) = Sys.opaque_identity padding_258 in
    let (_ : string) = Sys.opaque_identity padding_259 in
    let (_ : string) = Sys.opaque_identity padding_260 in
    let (_ : string) = Sys.opaque_identity padding_261 in
    let (_ : string) = Sys.opaque_identity padding_262 in
    let (_ : string) = Sys.opaque_identity padding_263 in
    let (_ : string) = Sys.opaque_identity padding_264 in
    let (_ : string) = Sys.opaque_identity padding_265 in
    let (_ : string) = Sys.opaque_identity padding_266 in
    let (_ : string) = Sys.opaque_identity padding_267 in
    let (_ : string) = Sys.opaque_identity padding_268 in
    let (_ : string) = Sys.opaque_identity padding_269 in
    let (_ : string) = Sys.opaque_identity padding_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    200
  in
  (* Cases to exercise [Infix_tag] logic *)
  let[@opaque] rec rec_c1_1arg () =
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    i64_1
  and[@opaque] rec_c2_1arg () =
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    i64_2
  and[@opaque] rec_c3_1arg () =
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_1 in
    let (_ : string) = Sys.opaque_identity padding_2 in
    let (_ : string) = Sys.opaque_identity padding_3 in
    let (_ : string) = Sys.opaque_identity padding_4 in
    let (_ : string) = Sys.opaque_identity padding_5 in
    let (_ : string) = Sys.opaque_identity padding_6 in
    let (_ : string) = Sys.opaque_identity padding_7 in
    let (_ : string) = Sys.opaque_identity padding_8 in
    let (_ : string) = Sys.opaque_identity padding_9 in
    let (_ : string) = Sys.opaque_identity padding_10 in
    let (_ : string) = Sys.opaque_identity padding_11 in
    let (_ : string) = Sys.opaque_identity padding_12 in
    let (_ : string) = Sys.opaque_identity padding_13 in
    let (_ : string) = Sys.opaque_identity padding_14 in
    let (_ : string) = Sys.opaque_identity padding_15 in
    let (_ : string) = Sys.opaque_identity padding_16 in
    let (_ : string) = Sys.opaque_identity padding_17 in
    let (_ : string) = Sys.opaque_identity padding_18 in
    let (_ : string) = Sys.opaque_identity padding_19 in
    let (_ : string) = Sys.opaque_identity padding_20 in
    let (_ : string) = Sys.opaque_identity padding_21 in
    let (_ : string) = Sys.opaque_identity padding_22 in
    let (_ : string) = Sys.opaque_identity padding_23 in
    let (_ : string) = Sys.opaque_identity padding_24 in
    let (_ : string) = Sys.opaque_identity padding_25 in
    let (_ : string) = Sys.opaque_identity padding_26 in
    let (_ : string) = Sys.opaque_identity padding_27 in
    let (_ : string) = Sys.opaque_identity padding_28 in
    let (_ : string) = Sys.opaque_identity padding_29 in
    let (_ : string) = Sys.opaque_identity padding_30 in
    let (_ : string) = Sys.opaque_identity padding_31 in
    let (_ : string) = Sys.opaque_identity padding_32 in
    let (_ : string) = Sys.opaque_identity padding_33 in
    let (_ : string) = Sys.opaque_identity padding_34 in
    let (_ : string) = Sys.opaque_identity padding_35 in
    let (_ : string) = Sys.opaque_identity padding_36 in
    let (_ : string) = Sys.opaque_identity padding_37 in
    let (_ : string) = Sys.opaque_identity padding_38 in
    let (_ : string) = Sys.opaque_identity padding_39 in
    let (_ : string) = Sys.opaque_identity padding_40 in
    let (_ : string) = Sys.opaque_identity padding_41 in
    let (_ : string) = Sys.opaque_identity padding_42 in
    let (_ : string) = Sys.opaque_identity padding_43 in
    let (_ : string) = Sys.opaque_identity padding_44 in
    let (_ : string) = Sys.opaque_identity padding_45 in
    let (_ : string) = Sys.opaque_identity padding_46 in
    let (_ : string) = Sys.opaque_identity padding_47 in
    let (_ : string) = Sys.opaque_identity padding_48 in
    let (_ : string) = Sys.opaque_identity padding_49 in
    let (_ : string) = Sys.opaque_identity padding_50 in
    let (_ : string) = Sys.opaque_identity padding_51 in
    let (_ : string) = Sys.opaque_identity padding_52 in
    let (_ : string) = Sys.opaque_identity padding_53 in
    let (_ : string) = Sys.opaque_identity padding_54 in
    let (_ : string) = Sys.opaque_identity padding_55 in
    let (_ : string) = Sys.opaque_identity padding_56 in
    let (_ : string) = Sys.opaque_identity padding_57 in
    let (_ : string) = Sys.opaque_identity padding_58 in
    let (_ : string) = Sys.opaque_identity padding_59 in
    let (_ : string) = Sys.opaque_identity padding_60 in
    let (_ : string) = Sys.opaque_identity padding_61 in
    let (_ : string) = Sys.opaque_identity padding_62 in
    let (_ : string) = Sys.opaque_identity padding_63 in
    let (_ : string) = Sys.opaque_identity padding_64 in
    let (_ : string) = Sys.opaque_identity padding_65 in
    let (_ : string) = Sys.opaque_identity padding_66 in
    let (_ : string) = Sys.opaque_identity padding_67 in
    let (_ : string) = Sys.opaque_identity padding_68 in
    let (_ : string) = Sys.opaque_identity padding_69 in
    let (_ : string) = Sys.opaque_identity padding_70 in
    let (_ : string) = Sys.opaque_identity padding_71 in
    let (_ : string) = Sys.opaque_identity padding_72 in
    let (_ : string) = Sys.opaque_identity padding_73 in
    let (_ : string) = Sys.opaque_identity padding_74 in
    let (_ : string) = Sys.opaque_identity padding_75 in
    let (_ : string) = Sys.opaque_identity padding_76 in
    let (_ : string) = Sys.opaque_identity padding_77 in
    let (_ : string) = Sys.opaque_identity padding_78 in
    let (_ : string) = Sys.opaque_identity padding_79 in
    let (_ : string) = Sys.opaque_identity padding_80 in
    let (_ : string) = Sys.opaque_identity padding_81 in
    let (_ : string) = Sys.opaque_identity padding_82 in
    let (_ : string) = Sys.opaque_identity padding_83 in
    let (_ : string) = Sys.opaque_identity padding_84 in
    let (_ : string) = Sys.opaque_identity padding_85 in
    let (_ : string) = Sys.opaque_identity padding_86 in
    let (_ : string) = Sys.opaque_identity padding_87 in
    let (_ : string) = Sys.opaque_identity padding_88 in
    let (_ : string) = Sys.opaque_identity padding_89 in
    let (_ : string) = Sys.opaque_identity padding_90 in
    let (_ : string) = Sys.opaque_identity padding_91 in
    let (_ : string) = Sys.opaque_identity padding_92 in
    let (_ : string) = Sys.opaque_identity padding_93 in
    let (_ : string) = Sys.opaque_identity padding_94 in
    let (_ : string) = Sys.opaque_identity padding_95 in
    let (_ : string) = Sys.opaque_identity padding_96 in
    let (_ : string) = Sys.opaque_identity padding_97 in
    let (_ : string) = Sys.opaque_identity padding_98 in
    let (_ : string) = Sys.opaque_identity padding_99 in
    let (_ : string) = Sys.opaque_identity padding_100 in
    let (_ : string) = Sys.opaque_identity padding_101 in
    let (_ : string) = Sys.opaque_identity padding_102 in
    let (_ : string) = Sys.opaque_identity padding_103 in
    let (_ : string) = Sys.opaque_identity padding_104 in
    let (_ : string) = Sys.opaque_identity padding_105 in
    let (_ : string) = Sys.opaque_identity padding_106 in
    let (_ : string) = Sys.opaque_identity padding_107 in
    let (_ : string) = Sys.opaque_identity padding_108 in
    let (_ : string) = Sys.opaque_identity padding_109 in
    let (_ : string) = Sys.opaque_identity padding_110 in
    let (_ : string) = Sys.opaque_identity padding_111 in
    let (_ : string) = Sys.opaque_identity padding_112 in
    let (_ : string) = Sys.opaque_identity padding_113 in
    let (_ : string) = Sys.opaque_identity padding_114 in
    let (_ : string) = Sys.opaque_identity padding_115 in
    let (_ : string) = Sys.opaque_identity padding_116 in
    let (_ : string) = Sys.opaque_identity padding_117 in
    let (_ : string) = Sys.opaque_identity padding_118 in
    let (_ : string) = Sys.opaque_identity padding_119 in
    let (_ : string) = Sys.opaque_identity padding_120 in
    let (_ : string) = Sys.opaque_identity padding_121 in
    let (_ : string) = Sys.opaque_identity padding_122 in
    let (_ : string) = Sys.opaque_identity padding_123 in
    let (_ : string) = Sys.opaque_identity padding_124 in
    let (_ : string) = Sys.opaque_identity padding_125 in
    let (_ : string) = Sys.opaque_identity padding_126 in
    let (_ : string) = Sys.opaque_identity padding_127 in
    let (_ : string) = Sys.opaque_identity padding_128 in
    let (_ : string) = Sys.opaque_identity padding_129 in
    let (_ : string) = Sys.opaque_identity padding_130 in
    let (_ : string) = Sys.opaque_identity padding_131 in
    let (_ : string) = Sys.opaque_identity padding_132 in
    let (_ : string) = Sys.opaque_identity padding_133 in
    let (_ : string) = Sys.opaque_identity padding_134 in
    let (_ : string) = Sys.opaque_identity padding_135 in
    let (_ : string) = Sys.opaque_identity padding_136 in
    let (_ : string) = Sys.opaque_identity padding_137 in
    let (_ : string) = Sys.opaque_identity padding_138 in
    let (_ : string) = Sys.opaque_identity padding_139 in
    let (_ : string) = Sys.opaque_identity padding_140 in
    let (_ : string) = Sys.opaque_identity padding_141 in
    let (_ : string) = Sys.opaque_identity padding_142 in
    let (_ : string) = Sys.opaque_identity padding_143 in
    let (_ : string) = Sys.opaque_identity padding_144 in
    let (_ : string) = Sys.opaque_identity padding_145 in
    let (_ : string) = Sys.opaque_identity padding_146 in
    let (_ : string) = Sys.opaque_identity padding_147 in
    let (_ : string) = Sys.opaque_identity padding_148 in
    let (_ : string) = Sys.opaque_identity padding_149 in
    let (_ : string) = Sys.opaque_identity padding_150 in
    let (_ : string) = Sys.opaque_identity padding_151 in
    let (_ : string) = Sys.opaque_identity padding_152 in
    let (_ : string) = Sys.opaque_identity padding_153 in
    let (_ : string) = Sys.opaque_identity padding_154 in
    let (_ : string) = Sys.opaque_identity padding_155 in
    let (_ : string) = Sys.opaque_identity padding_156 in
    let (_ : string) = Sys.opaque_identity padding_157 in
    let (_ : string) = Sys.opaque_identity padding_158 in
    let (_ : string) = Sys.opaque_identity padding_159 in
    let (_ : string) = Sys.opaque_identity padding_160 in
    let (_ : string) = Sys.opaque_identity padding_161 in
    let (_ : string) = Sys.opaque_identity padding_162 in
    let (_ : string) = Sys.opaque_identity padding_163 in
    let (_ : string) = Sys.opaque_identity padding_164 in
    let (_ : string) = Sys.opaque_identity padding_165 in
    let (_ : string) = Sys.opaque_identity padding_166 in
    let (_ : string) = Sys.opaque_identity padding_167 in
    let (_ : string) = Sys.opaque_identity padding_168 in
    let (_ : string) = Sys.opaque_identity padding_169 in
    let (_ : string) = Sys.opaque_identity padding_170 in
    let (_ : string) = Sys.opaque_identity padding_171 in
    let (_ : string) = Sys.opaque_identity padding_172 in
    let (_ : string) = Sys.opaque_identity padding_173 in
    let (_ : string) = Sys.opaque_identity padding_174 in
    let (_ : string) = Sys.opaque_identity padding_175 in
    let (_ : string) = Sys.opaque_identity padding_176 in
    let (_ : string) = Sys.opaque_identity padding_177 in
    let (_ : string) = Sys.opaque_identity padding_178 in
    let (_ : string) = Sys.opaque_identity padding_179 in
    let (_ : string) = Sys.opaque_identity padding_180 in
    let (_ : string) = Sys.opaque_identity padding_181 in
    let (_ : string) = Sys.opaque_identity padding_182 in
    let (_ : string) = Sys.opaque_identity padding_183 in
    let (_ : string) = Sys.opaque_identity padding_184 in
    let (_ : string) = Sys.opaque_identity padding_185 in
    let (_ : string) = Sys.opaque_identity padding_186 in
    let (_ : string) = Sys.opaque_identity padding_187 in
    let (_ : string) = Sys.opaque_identity padding_188 in
    let (_ : string) = Sys.opaque_identity padding_189 in
    let (_ : string) = Sys.opaque_identity padding_190 in
    let (_ : string) = Sys.opaque_identity padding_191 in
    let (_ : string) = Sys.opaque_identity padding_192 in
    let (_ : string) = Sys.opaque_identity padding_193 in
    let (_ : string) = Sys.opaque_identity padding_194 in
    let (_ : string) = Sys.opaque_identity padding_195 in
    let (_ : string) = Sys.opaque_identity padding_196 in
    let (_ : string) = Sys.opaque_identity padding_197 in
    let (_ : string) = Sys.opaque_identity padding_198 in
    let (_ : string) = Sys.opaque_identity padding_199 in
    let (_ : string) = Sys.opaque_identity padding_200 in
    let (_ : string) = Sys.opaque_identity padding_201 in
    let (_ : string) = Sys.opaque_identity padding_202 in
    let (_ : string) = Sys.opaque_identity padding_203 in
    let (_ : string) = Sys.opaque_identity padding_204 in
    let (_ : string) = Sys.opaque_identity padding_205 in
    let (_ : string) = Sys.opaque_identity padding_206 in
    let (_ : string) = Sys.opaque_identity padding_207 in
    let (_ : string) = Sys.opaque_identity padding_208 in
    let (_ : string) = Sys.opaque_identity padding_209 in
    let (_ : string) = Sys.opaque_identity padding_210 in
    let (_ : string) = Sys.opaque_identity padding_211 in
    let (_ : string) = Sys.opaque_identity padding_212 in
    let (_ : string) = Sys.opaque_identity padding_213 in
    let (_ : string) = Sys.opaque_identity padding_214 in
    let (_ : string) = Sys.opaque_identity padding_215 in
    let (_ : string) = Sys.opaque_identity padding_216 in
    let (_ : string) = Sys.opaque_identity padding_217 in
    let (_ : string) = Sys.opaque_identity padding_218 in
    let (_ : string) = Sys.opaque_identity padding_219 in
    let (_ : string) = Sys.opaque_identity padding_220 in
    let (_ : string) = Sys.opaque_identity padding_221 in
    let (_ : string) = Sys.opaque_identity padding_222 in
    let (_ : string) = Sys.opaque_identity padding_223 in
    let (_ : string) = Sys.opaque_identity padding_224 in
    let (_ : string) = Sys.opaque_identity padding_225 in
    let (_ : string) = Sys.opaque_identity padding_226 in
    let (_ : string) = Sys.opaque_identity padding_227 in
    let (_ : string) = Sys.opaque_identity padding_228 in
    let (_ : string) = Sys.opaque_identity padding_229 in
    let (_ : string) = Sys.opaque_identity padding_230 in
    let (_ : string) = Sys.opaque_identity padding_231 in
    let (_ : string) = Sys.opaque_identity padding_232 in
    let (_ : string) = Sys.opaque_identity padding_233 in
    let (_ : string) = Sys.opaque_identity padding_234 in
    let (_ : string) = Sys.opaque_identity padding_235 in
    let (_ : string) = Sys.opaque_identity padding_236 in
    let (_ : string) = Sys.opaque_identity padding_237 in
    let (_ : string) = Sys.opaque_identity padding_238 in
    let (_ : string) = Sys.opaque_identity padding_239 in
    let (_ : string) = Sys.opaque_identity padding_240 in
    let (_ : string) = Sys.opaque_identity padding_241 in
    let (_ : string) = Sys.opaque_identity padding_242 in
    let (_ : string) = Sys.opaque_identity padding_243 in
    let (_ : string) = Sys.opaque_identity padding_244 in
    let (_ : string) = Sys.opaque_identity padding_245 in
    let (_ : string) = Sys.opaque_identity padding_246 in
    let (_ : string) = Sys.opaque_identity padding_247 in
    let (_ : string) = Sys.opaque_identity padding_248 in
    let (_ : string) = Sys.opaque_identity padding_249 in
    let (_ : string) = Sys.opaque_identity padding_250 in
    let (_ : string) = Sys.opaque_identity padding_251 in
    let (_ : string) = Sys.opaque_identity padding_252 in
    let (_ : string) = Sys.opaque_identity padding_253 in
    let (_ : string) = Sys.opaque_identity padding_254 in
    let (_ : string) = Sys.opaque_identity padding_255 in
    let (_ : string) = Sys.opaque_identity padding_256 in
    let (_ : string) = Sys.opaque_identity padding_257 in
    let (_ : string) = Sys.opaque_identity padding_258 in
    let (_ : string) = Sys.opaque_identity padding_259 in
    let (_ : string) = Sys.opaque_identity padding_260 in
    let (_ : string) = Sys.opaque_identity padding_261 in
    let (_ : string) = Sys.opaque_identity padding_262 in
    let (_ : string) = Sys.opaque_identity padding_263 in
    let (_ : string) = Sys.opaque_identity padding_264 in
    let (_ : string) = Sys.opaque_identity padding_265 in
    let (_ : string) = Sys.opaque_identity padding_266 in
    let (_ : string) = Sys.opaque_identity padding_267 in
    let (_ : string) = Sys.opaque_identity padding_268 in
    let (_ : string) = Sys.opaque_identity padding_269 in
    let (_ : string) = Sys.opaque_identity padding_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    300
  and[@opaque] rec_c1_2arg () () =
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    let (_ : int64#) = Sys.opaque_identity i64_3 in
    rec_c1_1arg ()
  and[@opaque] rec_c2_2arg () () =
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_0 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_1 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_2 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_3 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_4 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_5 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_6 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_7 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_8 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_9 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_10 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_11 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_12 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_13 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_14 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_15 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_16 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_17 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_18 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_19 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_20 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_21 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_22 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_23 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_24 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_25 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_26 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_27 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_28 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_29 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_30 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_31 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_32 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_33 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_34 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_35 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_36 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_37 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_38 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_39 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_40 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_41 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_42 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_43 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_44 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_45 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_46 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_47 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_48 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_49 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_50 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_51 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_52 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_53 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_54 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_55 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_56 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_57 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_58 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_59 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_60 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_61 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_62 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_63 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_64 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_65 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_66 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_67 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_68 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_69 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_70 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_71 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_72 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_73 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_74 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_75 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_76 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_77 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_78 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_79 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_80 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_81 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_82 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_83 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_84 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_85 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_86 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_87 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_88 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_89 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_90 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_91 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_92 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_93 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_94 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_95 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_96 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_97 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_98 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_99 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_100 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_101 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_102 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_103 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_104 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_105 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_106 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_107 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_108 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_109 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_110 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_111 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_112 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_113 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_114 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_115 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_116 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_117 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_118 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_119 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_120 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_121 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_122 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_123 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_124 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_125 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_126 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_127 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_128 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_129 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_130 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_131 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_132 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_133 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_134 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_135 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_136 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_137 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_138 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_139 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_140 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_141 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_142 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_143 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_144 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_145 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_146 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_147 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_148 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_149 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_150 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_151 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_152 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_153 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_154 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_155 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_156 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_157 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_158 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_159 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_160 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_161 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_162 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_163 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_164 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_165 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_166 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_167 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_168 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_169 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_170 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_171 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_172 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_173 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_174 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_175 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_176 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_177 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_178 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_179 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_180 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_181 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_182 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_183 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_184 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_185 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_186 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_187 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_188 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_189 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_190 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_191 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_192 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_193 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_194 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_195 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_196 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_197 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_198 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_199 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_200 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_201 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_202 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_203 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_204 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_205 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_206 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_207 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_208 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_209 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_210 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_211 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_212 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_213 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_214 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_215 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_216 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_217 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_218 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_219 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_220 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_221 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_222 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_223 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_224 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_225 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_226 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_227 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_228 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_229 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_230 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_231 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_232 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_233 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_234 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_235 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_236 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_237 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_238 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_239 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_240 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_241 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_242 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_243 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_244 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_245 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_246 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_247 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_248 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_249 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_250 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_251 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_252 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_253 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_254 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_255 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_256 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_257 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_258 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_259 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_260 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_261 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_262 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_263 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_264 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_265 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_266 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_267 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_268 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_269 in
    let (_ : int64#) = Sys.opaque_identity padding_i64_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    let (_ : int64#) = Sys.opaque_identity i64_4 in
    rec_c2_1arg ()
  and[@opaque] rec_c3_2arg () () =
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_0 in
    let (_ : string) = Sys.opaque_identity padding_1 in
    let (_ : string) = Sys.opaque_identity padding_2 in
    let (_ : string) = Sys.opaque_identity padding_3 in
    let (_ : string) = Sys.opaque_identity padding_4 in
    let (_ : string) = Sys.opaque_identity padding_5 in
    let (_ : string) = Sys.opaque_identity padding_6 in
    let (_ : string) = Sys.opaque_identity padding_7 in
    let (_ : string) = Sys.opaque_identity padding_8 in
    let (_ : string) = Sys.opaque_identity padding_9 in
    let (_ : string) = Sys.opaque_identity padding_10 in
    let (_ : string) = Sys.opaque_identity padding_11 in
    let (_ : string) = Sys.opaque_identity padding_12 in
    let (_ : string) = Sys.opaque_identity padding_13 in
    let (_ : string) = Sys.opaque_identity padding_14 in
    let (_ : string) = Sys.opaque_identity padding_15 in
    let (_ : string) = Sys.opaque_identity padding_16 in
    let (_ : string) = Sys.opaque_identity padding_17 in
    let (_ : string) = Sys.opaque_identity padding_18 in
    let (_ : string) = Sys.opaque_identity padding_19 in
    let (_ : string) = Sys.opaque_identity padding_20 in
    let (_ : string) = Sys.opaque_identity padding_21 in
    let (_ : string) = Sys.opaque_identity padding_22 in
    let (_ : string) = Sys.opaque_identity padding_23 in
    let (_ : string) = Sys.opaque_identity padding_24 in
    let (_ : string) = Sys.opaque_identity padding_25 in
    let (_ : string) = Sys.opaque_identity padding_26 in
    let (_ : string) = Sys.opaque_identity padding_27 in
    let (_ : string) = Sys.opaque_identity padding_28 in
    let (_ : string) = Sys.opaque_identity padding_29 in
    let (_ : string) = Sys.opaque_identity padding_30 in
    let (_ : string) = Sys.opaque_identity padding_31 in
    let (_ : string) = Sys.opaque_identity padding_32 in
    let (_ : string) = Sys.opaque_identity padding_33 in
    let (_ : string) = Sys.opaque_identity padding_34 in
    let (_ : string) = Sys.opaque_identity padding_35 in
    let (_ : string) = Sys.opaque_identity padding_36 in
    let (_ : string) = Sys.opaque_identity padding_37 in
    let (_ : string) = Sys.opaque_identity padding_38 in
    let (_ : string) = Sys.opaque_identity padding_39 in
    let (_ : string) = Sys.opaque_identity padding_40 in
    let (_ : string) = Sys.opaque_identity padding_41 in
    let (_ : string) = Sys.opaque_identity padding_42 in
    let (_ : string) = Sys.opaque_identity padding_43 in
    let (_ : string) = Sys.opaque_identity padding_44 in
    let (_ : string) = Sys.opaque_identity padding_45 in
    let (_ : string) = Sys.opaque_identity padding_46 in
    let (_ : string) = Sys.opaque_identity padding_47 in
    let (_ : string) = Sys.opaque_identity padding_48 in
    let (_ : string) = Sys.opaque_identity padding_49 in
    let (_ : string) = Sys.opaque_identity padding_50 in
    let (_ : string) = Sys.opaque_identity padding_51 in
    let (_ : string) = Sys.opaque_identity padding_52 in
    let (_ : string) = Sys.opaque_identity padding_53 in
    let (_ : string) = Sys.opaque_identity padding_54 in
    let (_ : string) = Sys.opaque_identity padding_55 in
    let (_ : string) = Sys.opaque_identity padding_56 in
    let (_ : string) = Sys.opaque_identity padding_57 in
    let (_ : string) = Sys.opaque_identity padding_58 in
    let (_ : string) = Sys.opaque_identity padding_59 in
    let (_ : string) = Sys.opaque_identity padding_60 in
    let (_ : string) = Sys.opaque_identity padding_61 in
    let (_ : string) = Sys.opaque_identity padding_62 in
    let (_ : string) = Sys.opaque_identity padding_63 in
    let (_ : string) = Sys.opaque_identity padding_64 in
    let (_ : string) = Sys.opaque_identity padding_65 in
    let (_ : string) = Sys.opaque_identity padding_66 in
    let (_ : string) = Sys.opaque_identity padding_67 in
    let (_ : string) = Sys.opaque_identity padding_68 in
    let (_ : string) = Sys.opaque_identity padding_69 in
    let (_ : string) = Sys.opaque_identity padding_70 in
    let (_ : string) = Sys.opaque_identity padding_71 in
    let (_ : string) = Sys.opaque_identity padding_72 in
    let (_ : string) = Sys.opaque_identity padding_73 in
    let (_ : string) = Sys.opaque_identity padding_74 in
    let (_ : string) = Sys.opaque_identity padding_75 in
    let (_ : string) = Sys.opaque_identity padding_76 in
    let (_ : string) = Sys.opaque_identity padding_77 in
    let (_ : string) = Sys.opaque_identity padding_78 in
    let (_ : string) = Sys.opaque_identity padding_79 in
    let (_ : string) = Sys.opaque_identity padding_80 in
    let (_ : string) = Sys.opaque_identity padding_81 in
    let (_ : string) = Sys.opaque_identity padding_82 in
    let (_ : string) = Sys.opaque_identity padding_83 in
    let (_ : string) = Sys.opaque_identity padding_84 in
    let (_ : string) = Sys.opaque_identity padding_85 in
    let (_ : string) = Sys.opaque_identity padding_86 in
    let (_ : string) = Sys.opaque_identity padding_87 in
    let (_ : string) = Sys.opaque_identity padding_88 in
    let (_ : string) = Sys.opaque_identity padding_89 in
    let (_ : string) = Sys.opaque_identity padding_90 in
    let (_ : string) = Sys.opaque_identity padding_91 in
    let (_ : string) = Sys.opaque_identity padding_92 in
    let (_ : string) = Sys.opaque_identity padding_93 in
    let (_ : string) = Sys.opaque_identity padding_94 in
    let (_ : string) = Sys.opaque_identity padding_95 in
    let (_ : string) = Sys.opaque_identity padding_96 in
    let (_ : string) = Sys.opaque_identity padding_97 in
    let (_ : string) = Sys.opaque_identity padding_98 in
    let (_ : string) = Sys.opaque_identity padding_99 in
    let (_ : string) = Sys.opaque_identity padding_100 in
    let (_ : string) = Sys.opaque_identity padding_101 in
    let (_ : string) = Sys.opaque_identity padding_102 in
    let (_ : string) = Sys.opaque_identity padding_103 in
    let (_ : string) = Sys.opaque_identity padding_104 in
    let (_ : string) = Sys.opaque_identity padding_105 in
    let (_ : string) = Sys.opaque_identity padding_106 in
    let (_ : string) = Sys.opaque_identity padding_107 in
    let (_ : string) = Sys.opaque_identity padding_108 in
    let (_ : string) = Sys.opaque_identity padding_109 in
    let (_ : string) = Sys.opaque_identity padding_110 in
    let (_ : string) = Sys.opaque_identity padding_111 in
    let (_ : string) = Sys.opaque_identity padding_112 in
    let (_ : string) = Sys.opaque_identity padding_113 in
    let (_ : string) = Sys.opaque_identity padding_114 in
    let (_ : string) = Sys.opaque_identity padding_115 in
    let (_ : string) = Sys.opaque_identity padding_116 in
    let (_ : string) = Sys.opaque_identity padding_117 in
    let (_ : string) = Sys.opaque_identity padding_118 in
    let (_ : string) = Sys.opaque_identity padding_119 in
    let (_ : string) = Sys.opaque_identity padding_120 in
    let (_ : string) = Sys.opaque_identity padding_121 in
    let (_ : string) = Sys.opaque_identity padding_122 in
    let (_ : string) = Sys.opaque_identity padding_123 in
    let (_ : string) = Sys.opaque_identity padding_124 in
    let (_ : string) = Sys.opaque_identity padding_125 in
    let (_ : string) = Sys.opaque_identity padding_126 in
    let (_ : string) = Sys.opaque_identity padding_127 in
    let (_ : string) = Sys.opaque_identity padding_128 in
    let (_ : string) = Sys.opaque_identity padding_129 in
    let (_ : string) = Sys.opaque_identity padding_130 in
    let (_ : string) = Sys.opaque_identity padding_131 in
    let (_ : string) = Sys.opaque_identity padding_132 in
    let (_ : string) = Sys.opaque_identity padding_133 in
    let (_ : string) = Sys.opaque_identity padding_134 in
    let (_ : string) = Sys.opaque_identity padding_135 in
    let (_ : string) = Sys.opaque_identity padding_136 in
    let (_ : string) = Sys.opaque_identity padding_137 in
    let (_ : string) = Sys.opaque_identity padding_138 in
    let (_ : string) = Sys.opaque_identity padding_139 in
    let (_ : string) = Sys.opaque_identity padding_140 in
    let (_ : string) = Sys.opaque_identity padding_141 in
    let (_ : string) = Sys.opaque_identity padding_142 in
    let (_ : string) = Sys.opaque_identity padding_143 in
    let (_ : string) = Sys.opaque_identity padding_144 in
    let (_ : string) = Sys.opaque_identity padding_145 in
    let (_ : string) = Sys.opaque_identity padding_146 in
    let (_ : string) = Sys.opaque_identity padding_147 in
    let (_ : string) = Sys.opaque_identity padding_148 in
    let (_ : string) = Sys.opaque_identity padding_149 in
    let (_ : string) = Sys.opaque_identity padding_150 in
    let (_ : string) = Sys.opaque_identity padding_151 in
    let (_ : string) = Sys.opaque_identity padding_152 in
    let (_ : string) = Sys.opaque_identity padding_153 in
    let (_ : string) = Sys.opaque_identity padding_154 in
    let (_ : string) = Sys.opaque_identity padding_155 in
    let (_ : string) = Sys.opaque_identity padding_156 in
    let (_ : string) = Sys.opaque_identity padding_157 in
    let (_ : string) = Sys.opaque_identity padding_158 in
    let (_ : string) = Sys.opaque_identity padding_159 in
    let (_ : string) = Sys.opaque_identity padding_160 in
    let (_ : string) = Sys.opaque_identity padding_161 in
    let (_ : string) = Sys.opaque_identity padding_162 in
    let (_ : string) = Sys.opaque_identity padding_163 in
    let (_ : string) = Sys.opaque_identity padding_164 in
    let (_ : string) = Sys.opaque_identity padding_165 in
    let (_ : string) = Sys.opaque_identity padding_166 in
    let (_ : string) = Sys.opaque_identity padding_167 in
    let (_ : string) = Sys.opaque_identity padding_168 in
    let (_ : string) = Sys.opaque_identity padding_169 in
    let (_ : string) = Sys.opaque_identity padding_170 in
    let (_ : string) = Sys.opaque_identity padding_171 in
    let (_ : string) = Sys.opaque_identity padding_172 in
    let (_ : string) = Sys.opaque_identity padding_173 in
    let (_ : string) = Sys.opaque_identity padding_174 in
    let (_ : string) = Sys.opaque_identity padding_175 in
    let (_ : string) = Sys.opaque_identity padding_176 in
    let (_ : string) = Sys.opaque_identity padding_177 in
    let (_ : string) = Sys.opaque_identity padding_178 in
    let (_ : string) = Sys.opaque_identity padding_179 in
    let (_ : string) = Sys.opaque_identity padding_180 in
    let (_ : string) = Sys.opaque_identity padding_181 in
    let (_ : string) = Sys.opaque_identity padding_182 in
    let (_ : string) = Sys.opaque_identity padding_183 in
    let (_ : string) = Sys.opaque_identity padding_184 in
    let (_ : string) = Sys.opaque_identity padding_185 in
    let (_ : string) = Sys.opaque_identity padding_186 in
    let (_ : string) = Sys.opaque_identity padding_187 in
    let (_ : string) = Sys.opaque_identity padding_188 in
    let (_ : string) = Sys.opaque_identity padding_189 in
    let (_ : string) = Sys.opaque_identity padding_190 in
    let (_ : string) = Sys.opaque_identity padding_191 in
    let (_ : string) = Sys.opaque_identity padding_192 in
    let (_ : string) = Sys.opaque_identity padding_193 in
    let (_ : string) = Sys.opaque_identity padding_194 in
    let (_ : string) = Sys.opaque_identity padding_195 in
    let (_ : string) = Sys.opaque_identity padding_196 in
    let (_ : string) = Sys.opaque_identity padding_197 in
    let (_ : string) = Sys.opaque_identity padding_198 in
    let (_ : string) = Sys.opaque_identity padding_199 in
    let (_ : string) = Sys.opaque_identity padding_200 in
    let (_ : string) = Sys.opaque_identity padding_201 in
    let (_ : string) = Sys.opaque_identity padding_202 in
    let (_ : string) = Sys.opaque_identity padding_203 in
    let (_ : string) = Sys.opaque_identity padding_204 in
    let (_ : string) = Sys.opaque_identity padding_205 in
    let (_ : string) = Sys.opaque_identity padding_206 in
    let (_ : string) = Sys.opaque_identity padding_207 in
    let (_ : string) = Sys.opaque_identity padding_208 in
    let (_ : string) = Sys.opaque_identity padding_209 in
    let (_ : string) = Sys.opaque_identity padding_210 in
    let (_ : string) = Sys.opaque_identity padding_211 in
    let (_ : string) = Sys.opaque_identity padding_212 in
    let (_ : string) = Sys.opaque_identity padding_213 in
    let (_ : string) = Sys.opaque_identity padding_214 in
    let (_ : string) = Sys.opaque_identity padding_215 in
    let (_ : string) = Sys.opaque_identity padding_216 in
    let (_ : string) = Sys.opaque_identity padding_217 in
    let (_ : string) = Sys.opaque_identity padding_218 in
    let (_ : string) = Sys.opaque_identity padding_219 in
    let (_ : string) = Sys.opaque_identity padding_220 in
    let (_ : string) = Sys.opaque_identity padding_221 in
    let (_ : string) = Sys.opaque_identity padding_222 in
    let (_ : string) = Sys.opaque_identity padding_223 in
    let (_ : string) = Sys.opaque_identity padding_224 in
    let (_ : string) = Sys.opaque_identity padding_225 in
    let (_ : string) = Sys.opaque_identity padding_226 in
    let (_ : string) = Sys.opaque_identity padding_227 in
    let (_ : string) = Sys.opaque_identity padding_228 in
    let (_ : string) = Sys.opaque_identity padding_229 in
    let (_ : string) = Sys.opaque_identity padding_230 in
    let (_ : string) = Sys.opaque_identity padding_231 in
    let (_ : string) = Sys.opaque_identity padding_232 in
    let (_ : string) = Sys.opaque_identity padding_233 in
    let (_ : string) = Sys.opaque_identity padding_234 in
    let (_ : string) = Sys.opaque_identity padding_235 in
    let (_ : string) = Sys.opaque_identity padding_236 in
    let (_ : string) = Sys.opaque_identity padding_237 in
    let (_ : string) = Sys.opaque_identity padding_238 in
    let (_ : string) = Sys.opaque_identity padding_239 in
    let (_ : string) = Sys.opaque_identity padding_240 in
    let (_ : string) = Sys.opaque_identity padding_241 in
    let (_ : string) = Sys.opaque_identity padding_242 in
    let (_ : string) = Sys.opaque_identity padding_243 in
    let (_ : string) = Sys.opaque_identity padding_244 in
    let (_ : string) = Sys.opaque_identity padding_245 in
    let (_ : string) = Sys.opaque_identity padding_246 in
    let (_ : string) = Sys.opaque_identity padding_247 in
    let (_ : string) = Sys.opaque_identity padding_248 in
    let (_ : string) = Sys.opaque_identity padding_249 in
    let (_ : string) = Sys.opaque_identity padding_250 in
    let (_ : string) = Sys.opaque_identity padding_251 in
    let (_ : string) = Sys.opaque_identity padding_252 in
    let (_ : string) = Sys.opaque_identity padding_253 in
    let (_ : string) = Sys.opaque_identity padding_254 in
    let (_ : string) = Sys.opaque_identity padding_255 in
    let (_ : string) = Sys.opaque_identity padding_256 in
    let (_ : string) = Sys.opaque_identity padding_257 in
    let (_ : string) = Sys.opaque_identity padding_258 in
    let (_ : string) = Sys.opaque_identity padding_259 in
    let (_ : string) = Sys.opaque_identity padding_260 in
    let (_ : string) = Sys.opaque_identity padding_261 in
    let (_ : string) = Sys.opaque_identity padding_262 in
    let (_ : string) = Sys.opaque_identity padding_263 in
    let (_ : string) = Sys.opaque_identity padding_264 in
    let (_ : string) = Sys.opaque_identity padding_265 in
    let (_ : string) = Sys.opaque_identity padding_266 in
    let (_ : string) = Sys.opaque_identity padding_267 in
    let (_ : string) = Sys.opaque_identity padding_268 in
    let (_ : string) = Sys.opaque_identity padding_269 in
    let (_ : string) = Sys.opaque_identity padding_270 in
    let (_i : int) = Sys.opaque_identity (String.length x) in
    rec_c3_1arg ()
  in
  ( c1_1arg,
    c2_1arg,
    c3_1arg,
    c1_2arg,
    c2_2arg,
    c3_2arg,
    rec_c1_1arg,
    rec_c2_1arg,
    rec_c3_1arg,
    rec_c1_2arg,
    rec_c2_2arg,
    rec_c3_2arg )

let check_one_large_closures () =
  let i64_1 = rand_near_minor_heap () in
  let i64_2 = rand_near_minor_heap () in
  let i64_3 = rand_near_minor_heap () in
  let i64_4 = rand_near_minor_heap () in
  let padding_i64_0 = rand_near_minor_heap () in
  let padding_i64_1 = rand_near_minor_heap () in
  let padding_i64_2 = rand_near_minor_heap () in
  let padding_i64_3 = rand_near_minor_heap () in
  let padding_i64_4 = rand_near_minor_heap () in
  let padding_i64_5 = rand_near_minor_heap () in
  let padding_i64_6 = rand_near_minor_heap () in
  let padding_i64_7 = rand_near_minor_heap () in
  let padding_i64_8 = rand_near_minor_heap () in
  let padding_i64_9 = rand_near_minor_heap () in
  let padding_i64_10 = rand_near_minor_heap () in
  let padding_i64_11 = rand_near_minor_heap () in
  let padding_i64_12 = rand_near_minor_heap () in
  let padding_i64_13 = rand_near_minor_heap () in
  let padding_i64_14 = rand_near_minor_heap () in
  let padding_i64_15 = rand_near_minor_heap () in
  let padding_i64_16 = rand_near_minor_heap () in
  let padding_i64_17 = rand_near_minor_heap () in
  let padding_i64_18 = rand_near_minor_heap () in
  let padding_i64_19 = rand_near_minor_heap () in
  let padding_i64_20 = rand_near_minor_heap () in
  let padding_i64_21 = rand_near_minor_heap () in
  let padding_i64_22 = rand_near_minor_heap () in
  let padding_i64_23 = rand_near_minor_heap () in
  let padding_i64_24 = rand_near_minor_heap () in
  let padding_i64_25 = rand_near_minor_heap () in
  let padding_i64_26 = rand_near_minor_heap () in
  let padding_i64_27 = rand_near_minor_heap () in
  let padding_i64_28 = rand_near_minor_heap () in
  let padding_i64_29 = rand_near_minor_heap () in
  let padding_i64_30 = rand_near_minor_heap () in
  let padding_i64_31 = rand_near_minor_heap () in
  let padding_i64_32 = rand_near_minor_heap () in
  let padding_i64_33 = rand_near_minor_heap () in
  let padding_i64_34 = rand_near_minor_heap () in
  let padding_i64_35 = rand_near_minor_heap () in
  let padding_i64_36 = rand_near_minor_heap () in
  let padding_i64_37 = rand_near_minor_heap () in
  let padding_i64_38 = rand_near_minor_heap () in
  let padding_i64_39 = rand_near_minor_heap () in
  let padding_i64_40 = rand_near_minor_heap () in
  let padding_i64_41 = rand_near_minor_heap () in
  let padding_i64_42 = rand_near_minor_heap () in
  let padding_i64_43 = rand_near_minor_heap () in
  let padding_i64_44 = rand_near_minor_heap () in
  let padding_i64_45 = rand_near_minor_heap () in
  let padding_i64_46 = rand_near_minor_heap () in
  let padding_i64_47 = rand_near_minor_heap () in
  let padding_i64_48 = rand_near_minor_heap () in
  let padding_i64_49 = rand_near_minor_heap () in
  let padding_i64_50 = rand_near_minor_heap () in
  let padding_i64_51 = rand_near_minor_heap () in
  let padding_i64_52 = rand_near_minor_heap () in
  let padding_i64_53 = rand_near_minor_heap () in
  let padding_i64_54 = rand_near_minor_heap () in
  let padding_i64_55 = rand_near_minor_heap () in
  let padding_i64_56 = rand_near_minor_heap () in
  let padding_i64_57 = rand_near_minor_heap () in
  let padding_i64_58 = rand_near_minor_heap () in
  let padding_i64_59 = rand_near_minor_heap () in
  let padding_i64_60 = rand_near_minor_heap () in
  let padding_i64_61 = rand_near_minor_heap () in
  let padding_i64_62 = rand_near_minor_heap () in
  let padding_i64_63 = rand_near_minor_heap () in
  let padding_i64_64 = rand_near_minor_heap () in
  let padding_i64_65 = rand_near_minor_heap () in
  let padding_i64_66 = rand_near_minor_heap () in
  let padding_i64_67 = rand_near_minor_heap () in
  let padding_i64_68 = rand_near_minor_heap () in
  let padding_i64_69 = rand_near_minor_heap () in
  let padding_i64_70 = rand_near_minor_heap () in
  let padding_i64_71 = rand_near_minor_heap () in
  let padding_i64_72 = rand_near_minor_heap () in
  let padding_i64_73 = rand_near_minor_heap () in
  let padding_i64_74 = rand_near_minor_heap () in
  let padding_i64_75 = rand_near_minor_heap () in
  let padding_i64_76 = rand_near_minor_heap () in
  let padding_i64_77 = rand_near_minor_heap () in
  let padding_i64_78 = rand_near_minor_heap () in
  let padding_i64_79 = rand_near_minor_heap () in
  let padding_i64_80 = rand_near_minor_heap () in
  let padding_i64_81 = rand_near_minor_heap () in
  let padding_i64_82 = rand_near_minor_heap () in
  let padding_i64_83 = rand_near_minor_heap () in
  let padding_i64_84 = rand_near_minor_heap () in
  let padding_i64_85 = rand_near_minor_heap () in
  let padding_i64_86 = rand_near_minor_heap () in
  let padding_i64_87 = rand_near_minor_heap () in
  let padding_i64_88 = rand_near_minor_heap () in
  let padding_i64_89 = rand_near_minor_heap () in
  let padding_i64_90 = rand_near_minor_heap () in
  let padding_i64_91 = rand_near_minor_heap () in
  let padding_i64_92 = rand_near_minor_heap () in
  let padding_i64_93 = rand_near_minor_heap () in
  let padding_i64_94 = rand_near_minor_heap () in
  let padding_i64_95 = rand_near_minor_heap () in
  let padding_i64_96 = rand_near_minor_heap () in
  let padding_i64_97 = rand_near_minor_heap () in
  let padding_i64_98 = rand_near_minor_heap () in
  let padding_i64_99 = rand_near_minor_heap () in
  let padding_i64_100 = rand_near_minor_heap () in
  let padding_i64_101 = rand_near_minor_heap () in
  let padding_i64_102 = rand_near_minor_heap () in
  let padding_i64_103 = rand_near_minor_heap () in
  let padding_i64_104 = rand_near_minor_heap () in
  let padding_i64_105 = rand_near_minor_heap () in
  let padding_i64_106 = rand_near_minor_heap () in
  let padding_i64_107 = rand_near_minor_heap () in
  let padding_i64_108 = rand_near_minor_heap () in
  let padding_i64_109 = rand_near_minor_heap () in
  let padding_i64_110 = rand_near_minor_heap () in
  let padding_i64_111 = rand_near_minor_heap () in
  let padding_i64_112 = rand_near_minor_heap () in
  let padding_i64_113 = rand_near_minor_heap () in
  let padding_i64_114 = rand_near_minor_heap () in
  let padding_i64_115 = rand_near_minor_heap () in
  let padding_i64_116 = rand_near_minor_heap () in
  let padding_i64_117 = rand_near_minor_heap () in
  let padding_i64_118 = rand_near_minor_heap () in
  let padding_i64_119 = rand_near_minor_heap () in
  let padding_i64_120 = rand_near_minor_heap () in
  let padding_i64_121 = rand_near_minor_heap () in
  let padding_i64_122 = rand_near_minor_heap () in
  let padding_i64_123 = rand_near_minor_heap () in
  let padding_i64_124 = rand_near_minor_heap () in
  let padding_i64_125 = rand_near_minor_heap () in
  let padding_i64_126 = rand_near_minor_heap () in
  let padding_i64_127 = rand_near_minor_heap () in
  let padding_i64_128 = rand_near_minor_heap () in
  let padding_i64_129 = rand_near_minor_heap () in
  let padding_i64_130 = rand_near_minor_heap () in
  let padding_i64_131 = rand_near_minor_heap () in
  let padding_i64_132 = rand_near_minor_heap () in
  let padding_i64_133 = rand_near_minor_heap () in
  let padding_i64_134 = rand_near_minor_heap () in
  let padding_i64_135 = rand_near_minor_heap () in
  let padding_i64_136 = rand_near_minor_heap () in
  let padding_i64_137 = rand_near_minor_heap () in
  let padding_i64_138 = rand_near_minor_heap () in
  let padding_i64_139 = rand_near_minor_heap () in
  let padding_i64_140 = rand_near_minor_heap () in
  let padding_i64_141 = rand_near_minor_heap () in
  let padding_i64_142 = rand_near_minor_heap () in
  let padding_i64_143 = rand_near_minor_heap () in
  let padding_i64_144 = rand_near_minor_heap () in
  let padding_i64_145 = rand_near_minor_heap () in
  let padding_i64_146 = rand_near_minor_heap () in
  let padding_i64_147 = rand_near_minor_heap () in
  let padding_i64_148 = rand_near_minor_heap () in
  let padding_i64_149 = rand_near_minor_heap () in
  let padding_i64_150 = rand_near_minor_heap () in
  let padding_i64_151 = rand_near_minor_heap () in
  let padding_i64_152 = rand_near_minor_heap () in
  let padding_i64_153 = rand_near_minor_heap () in
  let padding_i64_154 = rand_near_minor_heap () in
  let padding_i64_155 = rand_near_minor_heap () in
  let padding_i64_156 = rand_near_minor_heap () in
  let padding_i64_157 = rand_near_minor_heap () in
  let padding_i64_158 = rand_near_minor_heap () in
  let padding_i64_159 = rand_near_minor_heap () in
  let padding_i64_160 = rand_near_minor_heap () in
  let padding_i64_161 = rand_near_minor_heap () in
  let padding_i64_162 = rand_near_minor_heap () in
  let padding_i64_163 = rand_near_minor_heap () in
  let padding_i64_164 = rand_near_minor_heap () in
  let padding_i64_165 = rand_near_minor_heap () in
  let padding_i64_166 = rand_near_minor_heap () in
  let padding_i64_167 = rand_near_minor_heap () in
  let padding_i64_168 = rand_near_minor_heap () in
  let padding_i64_169 = rand_near_minor_heap () in
  let padding_i64_170 = rand_near_minor_heap () in
  let padding_i64_171 = rand_near_minor_heap () in
  let padding_i64_172 = rand_near_minor_heap () in
  let padding_i64_173 = rand_near_minor_heap () in
  let padding_i64_174 = rand_near_minor_heap () in
  let padding_i64_175 = rand_near_minor_heap () in
  let padding_i64_176 = rand_near_minor_heap () in
  let padding_i64_177 = rand_near_minor_heap () in
  let padding_i64_178 = rand_near_minor_heap () in
  let padding_i64_179 = rand_near_minor_heap () in
  let padding_i64_180 = rand_near_minor_heap () in
  let padding_i64_181 = rand_near_minor_heap () in
  let padding_i64_182 = rand_near_minor_heap () in
  let padding_i64_183 = rand_near_minor_heap () in
  let padding_i64_184 = rand_near_minor_heap () in
  let padding_i64_185 = rand_near_minor_heap () in
  let padding_i64_186 = rand_near_minor_heap () in
  let padding_i64_187 = rand_near_minor_heap () in
  let padding_i64_188 = rand_near_minor_heap () in
  let padding_i64_189 = rand_near_minor_heap () in
  let padding_i64_190 = rand_near_minor_heap () in
  let padding_i64_191 = rand_near_minor_heap () in
  let padding_i64_192 = rand_near_minor_heap () in
  let padding_i64_193 = rand_near_minor_heap () in
  let padding_i64_194 = rand_near_minor_heap () in
  let padding_i64_195 = rand_near_minor_heap () in
  let padding_i64_196 = rand_near_minor_heap () in
  let padding_i64_197 = rand_near_minor_heap () in
  let padding_i64_198 = rand_near_minor_heap () in
  let padding_i64_199 = rand_near_minor_heap () in
  let padding_i64_200 = rand_near_minor_heap () in
  let padding_i64_201 = rand_near_minor_heap () in
  let padding_i64_202 = rand_near_minor_heap () in
  let padding_i64_203 = rand_near_minor_heap () in
  let padding_i64_204 = rand_near_minor_heap () in
  let padding_i64_205 = rand_near_minor_heap () in
  let padding_i64_206 = rand_near_minor_heap () in
  let padding_i64_207 = rand_near_minor_heap () in
  let padding_i64_208 = rand_near_minor_heap () in
  let padding_i64_209 = rand_near_minor_heap () in
  let padding_i64_210 = rand_near_minor_heap () in
  let padding_i64_211 = rand_near_minor_heap () in
  let padding_i64_212 = rand_near_minor_heap () in
  let padding_i64_213 = rand_near_minor_heap () in
  let padding_i64_214 = rand_near_minor_heap () in
  let padding_i64_215 = rand_near_minor_heap () in
  let padding_i64_216 = rand_near_minor_heap () in
  let padding_i64_217 = rand_near_minor_heap () in
  let padding_i64_218 = rand_near_minor_heap () in
  let padding_i64_219 = rand_near_minor_heap () in
  let padding_i64_220 = rand_near_minor_heap () in
  let padding_i64_221 = rand_near_minor_heap () in
  let padding_i64_222 = rand_near_minor_heap () in
  let padding_i64_223 = rand_near_minor_heap () in
  let padding_i64_224 = rand_near_minor_heap () in
  let padding_i64_225 = rand_near_minor_heap () in
  let padding_i64_226 = rand_near_minor_heap () in
  let padding_i64_227 = rand_near_minor_heap () in
  let padding_i64_228 = rand_near_minor_heap () in
  let padding_i64_229 = rand_near_minor_heap () in
  let padding_i64_230 = rand_near_minor_heap () in
  let padding_i64_231 = rand_near_minor_heap () in
  let padding_i64_232 = rand_near_minor_heap () in
  let padding_i64_233 = rand_near_minor_heap () in
  let padding_i64_234 = rand_near_minor_heap () in
  let padding_i64_235 = rand_near_minor_heap () in
  let padding_i64_236 = rand_near_minor_heap () in
  let padding_i64_237 = rand_near_minor_heap () in
  let padding_i64_238 = rand_near_minor_heap () in
  let padding_i64_239 = rand_near_minor_heap () in
  let padding_i64_240 = rand_near_minor_heap () in
  let padding_i64_241 = rand_near_minor_heap () in
  let padding_i64_242 = rand_near_minor_heap () in
  let padding_i64_243 = rand_near_minor_heap () in
  let padding_i64_244 = rand_near_minor_heap () in
  let padding_i64_245 = rand_near_minor_heap () in
  let padding_i64_246 = rand_near_minor_heap () in
  let padding_i64_247 = rand_near_minor_heap () in
  let padding_i64_248 = rand_near_minor_heap () in
  let padding_i64_249 = rand_near_minor_heap () in
  let padding_i64_250 = rand_near_minor_heap () in
  let padding_i64_251 = rand_near_minor_heap () in
  let padding_i64_252 = rand_near_minor_heap () in
  let padding_i64_253 = rand_near_minor_heap () in
  let padding_i64_254 = rand_near_minor_heap () in
  let padding_i64_255 = rand_near_minor_heap () in
  let padding_i64_256 = rand_near_minor_heap () in
  let padding_i64_257 = rand_near_minor_heap () in
  let padding_i64_258 = rand_near_minor_heap () in
  let padding_i64_259 = rand_near_minor_heap () in
  let padding_i64_260 = rand_near_minor_heap () in
  let padding_i64_261 = rand_near_minor_heap () in
  let padding_i64_262 = rand_near_minor_heap () in
  let padding_i64_263 = rand_near_minor_heap () in
  let padding_i64_264 = rand_near_minor_heap () in
  let padding_i64_265 = rand_near_minor_heap () in
  let padding_i64_266 = rand_near_minor_heap () in
  let padding_i64_267 = rand_near_minor_heap () in
  let padding_i64_268 = rand_near_minor_heap () in
  let padding_i64_269 = rand_near_minor_heap () in
  let padding_i64_270 = rand_near_minor_heap () in
  let padding_0 = rand_string () in
  let padding_1 = rand_string () in
  let padding_2 = rand_string () in
  let padding_3 = rand_string () in
  let padding_4 = rand_string () in
  let padding_5 = rand_string () in
  let padding_6 = rand_string () in
  let padding_7 = rand_string () in
  let padding_8 = rand_string () in
  let padding_9 = rand_string () in
  let padding_10 = rand_string () in
  let padding_11 = rand_string () in
  let padding_12 = rand_string () in
  let padding_13 = rand_string () in
  let padding_14 = rand_string () in
  let padding_15 = rand_string () in
  let padding_16 = rand_string () in
  let padding_17 = rand_string () in
  let padding_18 = rand_string () in
  let padding_19 = rand_string () in
  let padding_20 = rand_string () in
  let padding_21 = rand_string () in
  let padding_22 = rand_string () in
  let padding_23 = rand_string () in
  let padding_24 = rand_string () in
  let padding_25 = rand_string () in
  let padding_26 = rand_string () in
  let padding_27 = rand_string () in
  let padding_28 = rand_string () in
  let padding_29 = rand_string () in
  let padding_30 = rand_string () in
  let padding_31 = rand_string () in
  let padding_32 = rand_string () in
  let padding_33 = rand_string () in
  let padding_34 = rand_string () in
  let padding_35 = rand_string () in
  let padding_36 = rand_string () in
  let padding_37 = rand_string () in
  let padding_38 = rand_string () in
  let padding_39 = rand_string () in
  let padding_40 = rand_string () in
  let padding_41 = rand_string () in
  let padding_42 = rand_string () in
  let padding_43 = rand_string () in
  let padding_44 = rand_string () in
  let padding_45 = rand_string () in
  let padding_46 = rand_string () in
  let padding_47 = rand_string () in
  let padding_48 = rand_string () in
  let padding_49 = rand_string () in
  let padding_50 = rand_string () in
  let padding_51 = rand_string () in
  let padding_52 = rand_string () in
  let padding_53 = rand_string () in
  let padding_54 = rand_string () in
  let padding_55 = rand_string () in
  let padding_56 = rand_string () in
  let padding_57 = rand_string () in
  let padding_58 = rand_string () in
  let padding_59 = rand_string () in
  let padding_60 = rand_string () in
  let padding_61 = rand_string () in
  let padding_62 = rand_string () in
  let padding_63 = rand_string () in
  let padding_64 = rand_string () in
  let padding_65 = rand_string () in
  let padding_66 = rand_string () in
  let padding_67 = rand_string () in
  let padding_68 = rand_string () in
  let padding_69 = rand_string () in
  let padding_70 = rand_string () in
  let padding_71 = rand_string () in
  let padding_72 = rand_string () in
  let padding_73 = rand_string () in
  let padding_74 = rand_string () in
  let padding_75 = rand_string () in
  let padding_76 = rand_string () in
  let padding_77 = rand_string () in
  let padding_78 = rand_string () in
  let padding_79 = rand_string () in
  let padding_80 = rand_string () in
  let padding_81 = rand_string () in
  let padding_82 = rand_string () in
  let padding_83 = rand_string () in
  let padding_84 = rand_string () in
  let padding_85 = rand_string () in
  let padding_86 = rand_string () in
  let padding_87 = rand_string () in
  let padding_88 = rand_string () in
  let padding_89 = rand_string () in
  let padding_90 = rand_string () in
  let padding_91 = rand_string () in
  let padding_92 = rand_string () in
  let padding_93 = rand_string () in
  let padding_94 = rand_string () in
  let padding_95 = rand_string () in
  let padding_96 = rand_string () in
  let padding_97 = rand_string () in
  let padding_98 = rand_string () in
  let padding_99 = rand_string () in
  let padding_100 = rand_string () in
  let padding_101 = rand_string () in
  let padding_102 = rand_string () in
  let padding_103 = rand_string () in
  let padding_104 = rand_string () in
  let padding_105 = rand_string () in
  let padding_106 = rand_string () in
  let padding_107 = rand_string () in
  let padding_108 = rand_string () in
  let padding_109 = rand_string () in
  let padding_110 = rand_string () in
  let padding_111 = rand_string () in
  let padding_112 = rand_string () in
  let padding_113 = rand_string () in
  let padding_114 = rand_string () in
  let padding_115 = rand_string () in
  let padding_116 = rand_string () in
  let padding_117 = rand_string () in
  let padding_118 = rand_string () in
  let padding_119 = rand_string () in
  let padding_120 = rand_string () in
  let padding_121 = rand_string () in
  let padding_122 = rand_string () in
  let padding_123 = rand_string () in
  let padding_124 = rand_string () in
  let padding_125 = rand_string () in
  let padding_126 = rand_string () in
  let padding_127 = rand_string () in
  let padding_128 = rand_string () in
  let padding_129 = rand_string () in
  let padding_130 = rand_string () in
  let padding_131 = rand_string () in
  let padding_132 = rand_string () in
  let padding_133 = rand_string () in
  let padding_134 = rand_string () in
  let padding_135 = rand_string () in
  let padding_136 = rand_string () in
  let padding_137 = rand_string () in
  let padding_138 = rand_string () in
  let padding_139 = rand_string () in
  let padding_140 = rand_string () in
  let padding_141 = rand_string () in
  let padding_142 = rand_string () in
  let padding_143 = rand_string () in
  let padding_144 = rand_string () in
  let padding_145 = rand_string () in
  let padding_146 = rand_string () in
  let padding_147 = rand_string () in
  let padding_148 = rand_string () in
  let padding_149 = rand_string () in
  let padding_150 = rand_string () in
  let padding_151 = rand_string () in
  let padding_152 = rand_string () in
  let padding_153 = rand_string () in
  let padding_154 = rand_string () in
  let padding_155 = rand_string () in
  let padding_156 = rand_string () in
  let padding_157 = rand_string () in
  let padding_158 = rand_string () in
  let padding_159 = rand_string () in
  let padding_160 = rand_string () in
  let padding_161 = rand_string () in
  let padding_162 = rand_string () in
  let padding_163 = rand_string () in
  let padding_164 = rand_string () in
  let padding_165 = rand_string () in
  let padding_166 = rand_string () in
  let padding_167 = rand_string () in
  let padding_168 = rand_string () in
  let padding_169 = rand_string () in
  let padding_170 = rand_string () in
  let padding_171 = rand_string () in
  let padding_172 = rand_string () in
  let padding_173 = rand_string () in
  let padding_174 = rand_string () in
  let padding_175 = rand_string () in
  let padding_176 = rand_string () in
  let padding_177 = rand_string () in
  let padding_178 = rand_string () in
  let padding_179 = rand_string () in
  let padding_180 = rand_string () in
  let padding_181 = rand_string () in
  let padding_182 = rand_string () in
  let padding_183 = rand_string () in
  let padding_184 = rand_string () in
  let padding_185 = rand_string () in
  let padding_186 = rand_string () in
  let padding_187 = rand_string () in
  let padding_188 = rand_string () in
  let padding_189 = rand_string () in
  let padding_190 = rand_string () in
  let padding_191 = rand_string () in
  let padding_192 = rand_string () in
  let padding_193 = rand_string () in
  let padding_194 = rand_string () in
  let padding_195 = rand_string () in
  let padding_196 = rand_string () in
  let padding_197 = rand_string () in
  let padding_198 = rand_string () in
  let padding_199 = rand_string () in
  let padding_200 = rand_string () in
  let padding_201 = rand_string () in
  let padding_202 = rand_string () in
  let padding_203 = rand_string () in
  let padding_204 = rand_string () in
  let padding_205 = rand_string () in
  let padding_206 = rand_string () in
  let padding_207 = rand_string () in
  let padding_208 = rand_string () in
  let padding_209 = rand_string () in
  let padding_210 = rand_string () in
  let padding_211 = rand_string () in
  let padding_212 = rand_string () in
  let padding_213 = rand_string () in
  let padding_214 = rand_string () in
  let padding_215 = rand_string () in
  let padding_216 = rand_string () in
  let padding_217 = rand_string () in
  let padding_218 = rand_string () in
  let padding_219 = rand_string () in
  let padding_220 = rand_string () in
  let padding_221 = rand_string () in
  let padding_222 = rand_string () in
  let padding_223 = rand_string () in
  let padding_224 = rand_string () in
  let padding_225 = rand_string () in
  let padding_226 = rand_string () in
  let padding_227 = rand_string () in
  let padding_228 = rand_string () in
  let padding_229 = rand_string () in
  let padding_230 = rand_string () in
  let padding_231 = rand_string () in
  let padding_232 = rand_string () in
  let padding_233 = rand_string () in
  let padding_234 = rand_string () in
  let padding_235 = rand_string () in
  let padding_236 = rand_string () in
  let padding_237 = rand_string () in
  let padding_238 = rand_string () in
  let padding_239 = rand_string () in
  let padding_240 = rand_string () in
  let padding_241 = rand_string () in
  let padding_242 = rand_string () in
  let padding_243 = rand_string () in
  let padding_244 = rand_string () in
  let padding_245 = rand_string () in
  let padding_246 = rand_string () in
  let padding_247 = rand_string () in
  let padding_248 = rand_string () in
  let padding_249 = rand_string () in
  let padding_250 = rand_string () in
  let padding_251 = rand_string () in
  let padding_252 = rand_string () in
  let padding_253 = rand_string () in
  let padding_254 = rand_string () in
  let padding_255 = rand_string () in
  let padding_256 = rand_string () in
  let padding_257 = rand_string () in
  let padding_258 = rand_string () in
  let padding_259 = rand_string () in
  let padding_260 = rand_string () in
  let padding_261 = rand_string () in
  let padding_262 = rand_string () in
  let padding_263 = rand_string () in
  let padding_264 = rand_string () in
  let padding_265 = rand_string () in
  let padding_266 = rand_string () in
  let padding_267 = rand_string () in
  let padding_268 = rand_string () in
  let padding_269 = rand_string () in
  let padding_270 = rand_string () in
  let x = rand_string () in
  let ( c1_1arg_original,
        c2_1arg_original,
        c3_1arg_original,
        c1_2arg_original,
        c2_2arg_original,
        c3_2arg_original,
        rec_c1_1arg_original,
        rec_c2_1arg_original,
        rec_c3_1arg_original,
        rec_c1_2arg_original,
        rec_c2_2arg_original,
        rec_c3_2arg_original ) =
    make_large_closures i64_1 i64_2 i64_3 i64_4
      padding_i64_0
      padding_i64_1
      padding_i64_2
      padding_i64_3
      padding_i64_4
      padding_i64_5
      padding_i64_6
      padding_i64_7
      padding_i64_8
      padding_i64_9
      padding_i64_10
      padding_i64_11
      padding_i64_12
      padding_i64_13
      padding_i64_14
      padding_i64_15
      padding_i64_16
      padding_i64_17
      padding_i64_18
      padding_i64_19
      padding_i64_20
      padding_i64_21
      padding_i64_22
      padding_i64_23
      padding_i64_24
      padding_i64_25
      padding_i64_26
      padding_i64_27
      padding_i64_28
      padding_i64_29
      padding_i64_30
      padding_i64_31
      padding_i64_32
      padding_i64_33
      padding_i64_34
      padding_i64_35
      padding_i64_36
      padding_i64_37
      padding_i64_38
      padding_i64_39
      padding_i64_40
      padding_i64_41
      padding_i64_42
      padding_i64_43
      padding_i64_44
      padding_i64_45
      padding_i64_46
      padding_i64_47
      padding_i64_48
      padding_i64_49
      padding_i64_50
      padding_i64_51
      padding_i64_52
      padding_i64_53
      padding_i64_54
      padding_i64_55
      padding_i64_56
      padding_i64_57
      padding_i64_58
      padding_i64_59
      padding_i64_60
      padding_i64_61
      padding_i64_62
      padding_i64_63
      padding_i64_64
      padding_i64_65
      padding_i64_66
      padding_i64_67
      padding_i64_68
      padding_i64_69
      padding_i64_70
      padding_i64_71
      padding_i64_72
      padding_i64_73
      padding_i64_74
      padding_i64_75
      padding_i64_76
      padding_i64_77
      padding_i64_78
      padding_i64_79
      padding_i64_80
      padding_i64_81
      padding_i64_82
      padding_i64_83
      padding_i64_84
      padding_i64_85
      padding_i64_86
      padding_i64_87
      padding_i64_88
      padding_i64_89
      padding_i64_90
      padding_i64_91
      padding_i64_92
      padding_i64_93
      padding_i64_94
      padding_i64_95
      padding_i64_96
      padding_i64_97
      padding_i64_98
      padding_i64_99
      padding_i64_100
      padding_i64_101
      padding_i64_102
      padding_i64_103
      padding_i64_104
      padding_i64_105
      padding_i64_106
      padding_i64_107
      padding_i64_108
      padding_i64_109
      padding_i64_110
      padding_i64_111
      padding_i64_112
      padding_i64_113
      padding_i64_114
      padding_i64_115
      padding_i64_116
      padding_i64_117
      padding_i64_118
      padding_i64_119
      padding_i64_120
      padding_i64_121
      padding_i64_122
      padding_i64_123
      padding_i64_124
      padding_i64_125
      padding_i64_126
      padding_i64_127
      padding_i64_128
      padding_i64_129
      padding_i64_130
      padding_i64_131
      padding_i64_132
      padding_i64_133
      padding_i64_134
      padding_i64_135
      padding_i64_136
      padding_i64_137
      padding_i64_138
      padding_i64_139
      padding_i64_140
      padding_i64_141
      padding_i64_142
      padding_i64_143
      padding_i64_144
      padding_i64_145
      padding_i64_146
      padding_i64_147
      padding_i64_148
      padding_i64_149
      padding_i64_150
      padding_i64_151
      padding_i64_152
      padding_i64_153
      padding_i64_154
      padding_i64_155
      padding_i64_156
      padding_i64_157
      padding_i64_158
      padding_i64_159
      padding_i64_160
      padding_i64_161
      padding_i64_162
      padding_i64_163
      padding_i64_164
      padding_i64_165
      padding_i64_166
      padding_i64_167
      padding_i64_168
      padding_i64_169
      padding_i64_170
      padding_i64_171
      padding_i64_172
      padding_i64_173
      padding_i64_174
      padding_i64_175
      padding_i64_176
      padding_i64_177
      padding_i64_178
      padding_i64_179
      padding_i64_180
      padding_i64_181
      padding_i64_182
      padding_i64_183
      padding_i64_184
      padding_i64_185
      padding_i64_186
      padding_i64_187
      padding_i64_188
      padding_i64_189
      padding_i64_190
      padding_i64_191
      padding_i64_192
      padding_i64_193
      padding_i64_194
      padding_i64_195
      padding_i64_196
      padding_i64_197
      padding_i64_198
      padding_i64_199
      padding_i64_200
      padding_i64_201
      padding_i64_202
      padding_i64_203
      padding_i64_204
      padding_i64_205
      padding_i64_206
      padding_i64_207
      padding_i64_208
      padding_i64_209
      padding_i64_210
      padding_i64_211
      padding_i64_212
      padding_i64_213
      padding_i64_214
      padding_i64_215
      padding_i64_216
      padding_i64_217
      padding_i64_218
      padding_i64_219
      padding_i64_220
      padding_i64_221
      padding_i64_222
      padding_i64_223
      padding_i64_224
      padding_i64_225
      padding_i64_226
      padding_i64_227
      padding_i64_228
      padding_i64_229
      padding_i64_230
      padding_i64_231
      padding_i64_232
      padding_i64_233
      padding_i64_234
      padding_i64_235
      padding_i64_236
      padding_i64_237
      padding_i64_238
      padding_i64_239
      padding_i64_240
      padding_i64_241
      padding_i64_242
      padding_i64_243
      padding_i64_244
      padding_i64_245
      padding_i64_246
      padding_i64_247
      padding_i64_248
      padding_i64_249
      padding_i64_250
      padding_i64_251
      padding_i64_252
      padding_i64_253
      padding_i64_254
      padding_i64_255
      padding_i64_256
      padding_i64_257
      padding_i64_258
      padding_i64_259
      padding_i64_260
      padding_i64_261
      padding_i64_262
      padding_i64_263
      padding_i64_264
      padding_i64_265
      padding_i64_266
      padding_i64_267
      padding_i64_268
      padding_i64_269
      padding_i64_270
      padding_0
      padding_1
      padding_2
      padding_3
      padding_4
      padding_5
      padding_6
      padding_7
      padding_8
      padding_9
      padding_10
      padding_11
      padding_12
      padding_13
      padding_14
      padding_15
      padding_16
      padding_17
      padding_18
      padding_19
      padding_20
      padding_21
      padding_22
      padding_23
      padding_24
      padding_25
      padding_26
      padding_27
      padding_28
      padding_29
      padding_30
      padding_31
      padding_32
      padding_33
      padding_34
      padding_35
      padding_36
      padding_37
      padding_38
      padding_39
      padding_40
      padding_41
      padding_42
      padding_43
      padding_44
      padding_45
      padding_46
      padding_47
      padding_48
      padding_49
      padding_50
      padding_51
      padding_52
      padding_53
      padding_54
      padding_55
      padding_56
      padding_57
      padding_58
      padding_59
      padding_60
      padding_61
      padding_62
      padding_63
      padding_64
      padding_65
      padding_66
      padding_67
      padding_68
      padding_69
      padding_70
      padding_71
      padding_72
      padding_73
      padding_74
      padding_75
      padding_76
      padding_77
      padding_78
      padding_79
      padding_80
      padding_81
      padding_82
      padding_83
      padding_84
      padding_85
      padding_86
      padding_87
      padding_88
      padding_89
      padding_90
      padding_91
      padding_92
      padding_93
      padding_94
      padding_95
      padding_96
      padding_97
      padding_98
      padding_99
      padding_100
      padding_101
      padding_102
      padding_103
      padding_104
      padding_105
      padding_106
      padding_107
      padding_108
      padding_109
      padding_110
      padding_111
      padding_112
      padding_113
      padding_114
      padding_115
      padding_116
      padding_117
      padding_118
      padding_119
      padding_120
      padding_121
      padding_122
      padding_123
      padding_124
      padding_125
      padding_126
      padding_127
      padding_128
      padding_129
      padding_130
      padding_131
      padding_132
      padding_133
      padding_134
      padding_135
      padding_136
      padding_137
      padding_138
      padding_139
      padding_140
      padding_141
      padding_142
      padding_143
      padding_144
      padding_145
      padding_146
      padding_147
      padding_148
      padding_149
      padding_150
      padding_151
      padding_152
      padding_153
      padding_154
      padding_155
      padding_156
      padding_157
      padding_158
      padding_159
      padding_160
      padding_161
      padding_162
      padding_163
      padding_164
      padding_165
      padding_166
      padding_167
      padding_168
      padding_169
      padding_170
      padding_171
      padding_172
      padding_173
      padding_174
      padding_175
      padding_176
      padding_177
      padding_178
      padding_179
      padding_180
      padding_181
      padding_182
      padding_183
      padding_184
      padding_185
      padding_186
      padding_187
      padding_188
      padding_189
      padding_190
      padding_191
      padding_192
      padding_193
      padding_194
      padding_195
      padding_196
      padding_197
      padding_198
      padding_199
      padding_200
      padding_201
      padding_202
      padding_203
      padding_204
      padding_205
      padding_206
      padding_207
      padding_208
      padding_209
      padding_210
      padding_211
      padding_212
      padding_213
      padding_214
      padding_215
      padding_216
      padding_217
      padding_218
      padding_219
      padding_220
      padding_221
      padding_222
      padding_223
      padding_224
      padding_225
      padding_226
      padding_227
      padding_228
      padding_229
      padding_230
      padding_231
      padding_232
      padding_233
      padding_234
      padding_235
      padding_236
      padding_237
      padding_238
      padding_239
      padding_240
      padding_241
      padding_242
      padding_243
      padding_244
      padding_245
      padding_246
      padding_247
      padding_248
      padding_249
      padding_250
      padding_251
      padding_252
      padding_253
      padding_254
      padding_255
      padding_256
      padding_257
      padding_258
      padding_259
      padding_260
      padding_261
      padding_262
      padding_263
      padding_264
      padding_265
      padding_266
      padding_267
      padding_268
      padding_269
      padding_270
      x
  in
  let dup (type a) (x : a) : a = Obj.(obj (dup (repr x))) in
  let c1_1arg = dup c1_1arg_original in
  let c2_1arg = dup c2_1arg_original in
  let c3_1arg = dup c3_1arg_original in
  let c1_2arg = dup c1_2arg_original in
  let c2_2arg = dup c2_2arg_original in
  let c3_2arg = dup c3_2arg_original in
  let rec_c1_1arg = dup rec_c1_1arg_original in
  let rec_c2_1arg = dup rec_c2_1arg_original in
  let rec_c3_1arg = dup rec_c3_1arg_original in
  let rec_c1_2arg = dup rec_c1_2arg_original in
  let rec_c2_2arg = dup rec_c2_2arg_original in
  let rec_c3_2arg = dup rec_c3_2arg_original in
  Gc.compact ();
  let check_tag_and_size v1 v2 =
    let v1 = Obj.repr v1 in
    let v2 = Obj.repr v2 in
    assert (Obj.tag v1 = Obj.tag v2);
    assert (Obj.size v1 = Obj.size v2)
  in
  check_tag_and_size c1_1arg c1_1arg_original;
  check_tag_and_size c2_1arg c2_1arg_original;
  check_tag_and_size c3_1arg c3_1arg_original;
  check_tag_and_size c1_2arg c1_2arg_original;
  check_tag_and_size c2_2arg c2_2arg_original;
  check_tag_and_size c3_2arg c3_2arg_original;
  check_tag_and_size rec_c1_1arg rec_c1_1arg_original;
  check_tag_and_size rec_c2_1arg rec_c2_1arg_original;
  check_tag_and_size rec_c3_1arg rec_c3_1arg_original;
  check_tag_and_size rec_c1_2arg rec_c1_2arg_original;
  check_tag_and_size rec_c2_2arg rec_c2_2arg_original;
  check_tag_and_size rec_c3_2arg rec_c3_2arg_original;
  check_results "large" i64_1 i64_2 i64_3 i64_4 x
    ( c1_1arg,
      c2_1arg,
      c3_1arg,
      c1_2arg,
      c2_2arg,
      c3_2arg,
      rec_c1_1arg,
      rec_c2_1arg,
      rec_c3_1arg,
      rec_c1_2arg,
      rec_c2_2arg,
      rec_c3_2arg )

let () =
  Random.init 123;
  for x = 1 to 10_000 do
    check_one_small_closures ();
    check_one_large_closures ()
  done
