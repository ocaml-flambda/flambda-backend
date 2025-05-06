(* Test that spilled registers of type [Valx2] are correctly registered with the
   GC.

   Need at least 16 registers of 128 bit to trigger the spill on amd64.

   Allocate enough to defeat comballoc that moves allocations to the beginning
   of the block and out of the live range of the register that this test is
   aiming to spill. Raise [vectorize-max-block-size] to force the resulting very
   long block to be vectorized. *)
type s =
  { mutable f0 : int64;
    mutable f1 : int64;
    mutable f2 : int64;
    mutable f3 : int64;
    mutable f4 : int64;
    mutable f5 : int64;
    mutable f6 : int64;
    mutable f7 : int64;
    mutable f8 : int64;
    mutable f9 : int64;
    mutable f10 : int64;
    mutable f11 : int64;
    mutable f12 : int64;
    mutable f13 : int64;
    mutable f14 : int64;
    mutable f15 : int64;
    mutable f16 : int64;
    mutable f17 : int64;
    mutable f18 : int64;
    mutable f19 : int64;
    mutable f20 : int64;
    mutable f21 : int64;
    mutable f22 : int64;
    mutable f23 : int64;
    mutable f24 : int64;
    mutable f25 : int64;
    mutable f26 : int64;
    mutable f27 : int64;
    mutable f28 : int64;
    mutable f29 : int64;
    mutable f30 : int64;
    mutable f31 : int64;
    mutable f32 : int64;
    mutable f33 : int64;
    mutable f34 : int64;
    mutable f35 : int64
  }

let ( + ) = Int64.add

let[@opaque] foo a =
  let f0 = a.f0 in
  let f1 = a.f1 in
  let f2 = a.f2 in
  let f3 = a.f3 in
  let f4 = a.f4 in
  let f5 = a.f5 in
  let f6 = a.f6 in
  let f7 = a.f7 in
  let f8 = a.f8 in
  let f9 = a.f9 in
  let f10 = a.f10 in
  let f11 = a.f11 in
  let f12 = a.f12 in
  let f13 = a.f13 in
  let f14 = a.f14 in
  let f15 = a.f15 in
  let f16 = a.f16 in
  let f17 = a.f17 in
  let f18 = a.f18 in
  let f19 = a.f19 in
  let f20 = a.f20 in
  let f21 = a.f21 in
  let f22 = a.f22 in
  let f23 = a.f23 in
  let f24 = a.f24 in
  let f25 = a.f25 in
  let f26 = a.f26 in
  let f27 = a.f27 in
  let f28 = a.f28 in
  let f29 = a.f29 in
  let f30 = a.f30 in
  let f31 = a.f31 in
  let f32 = a.f32 in
  let f33 = a.f33 in
  let f34 = a.f34 in
  let f35 = a.f35 in
  let d0 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d1 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d2 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d3 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d4 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d5 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d6 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d7 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d8 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  let d9 =
    { f0;
      f1;
      f2;
      f3;
      f4;
      f5;
      f6;
      f7;
      f8;
      f9;
      f10;
      f11;
      f12;
      f13;
      f14;
      f15;
      f16;
      f17;
      f18;
      f19;
      f20;
      f21;
      f22;
      f23;
      f24;
      f25;
      f26;
      f27;
      f28;
      f29;
      f30;
      f31;
      f32;
      f33;
      f34;
      f35
    }
  in
  d0, d1, d2, d3, d4, d5, d6, d7, d8, d9

let () =
  let a =
    { f0 = 0L;
      f1 = 1L;
      f2 = 2L;
      f3 = 3L;
      f4 = 4L;
      f5 = 5L;
      f6 = 6L;
      f7 = 7L;
      f8 = 8L;
      f9 = 9L;
      f10 = 10L;
      f11 = 11L;
      f12 = 12L;
      f13 = 13L;
      f14 = 14L;
      f15 = 15L;
      f16 = 16L;
      f17 = 17L;
      f18 = 18L;
      f19 = 19L;
      f20 = 20L;
      f21 = 21L;
      f22 = 22L;
      f23 = 23L;
      f24 = 24L;
      f25 = 25L;
      f26 = 26L;
      f27 = 27L;
      f28 = 28L;
      f29 = 29L;
      f30 = 30L;
      f31 = 31L;
      f32 = 32L;
      f33 = 0L;
      f34 = 0L;
      f35 = 0L
    }
  in
  (* Gc.set { (Gc.get()) with Gc.verbose = 0xd }; *)
  let rec loop n =
    if n = 0
    then ()
    else
      (* try to trigger GC inside foo *)
      let d0, d1, d2, d3, d4, d5, d6, d7, d8, d9 = foo a in
      assert (d0.f0 = d1.f0);
      assert (d0.f1 = d1.f1);
      assert (d0.f2 = d1.f2);
      assert (d0.f3 = d1.f3);
      assert (d0.f4 = d1.f4);
      assert (d0.f5 = d1.f5);
      assert (d0.f6 = d1.f6);
      assert (d0.f7 = d1.f7);
      assert (d0.f8 = d1.f8);
      assert (d0.f9 = d1.f9);
      assert (d0.f10 = d1.f10);
      assert (d0.f11 = d1.f11);
      assert (d0.f12 = d1.f12);
      assert (d0.f13 = d1.f13);
      assert (d0.f14 = d1.f14);
      assert (d0.f15 = d1.f15);
      assert (d0.f16 = d1.f16);
      assert (d0.f17 = d1.f17);
      assert (d0.f18 = d1.f18);
      assert (d0.f19 = d1.f19);
      assert (d0.f20 = d1.f20);
      assert (d0.f21 = d1.f21);
      assert (d0.f22 = d1.f22);
      assert (d0.f23 = d1.f23);
      assert (d0.f24 = d1.f24);
      assert (d0.f25 = d1.f25);
      assert (d0.f26 = d1.f26);
      assert (d0.f27 = d1.f27);
      assert (d0.f28 = d1.f28);
      assert (d0.f29 = d1.f29);
      assert (d0.f30 = d1.f30);
      assert (d0.f31 = d1.f31);
      assert (d0.f32 = d1.f32);
      assert (d0.f33 = d1.f33);
      assert (d0.f34 = d1.f34);
      assert (d0.f35 = d1.f35);
      assert (d0.f0 = d2.f0);
      assert (d0.f1 = d2.f1);
      assert (d0.f2 = d2.f2);
      assert (d0.f3 = d2.f3);
      assert (d0.f4 = d2.f4);
      assert (d0.f5 = d2.f5);
      assert (d0.f6 = d2.f6);
      assert (d0.f7 = d2.f7);
      assert (d0.f8 = d2.f8);
      assert (d0.f9 = d2.f9);
      assert (d0.f10 = d2.f10);
      assert (d0.f11 = d2.f11);
      assert (d0.f12 = d2.f12);
      assert (d0.f13 = d2.f13);
      assert (d0.f14 = d2.f14);
      assert (d0.f15 = d2.f15);
      assert (d0.f16 = d2.f16);
      assert (d0.f17 = d2.f17);
      assert (d0.f18 = d2.f18);
      assert (d0.f19 = d2.f19);
      assert (d0.f20 = d2.f20);
      assert (d0.f21 = d2.f21);
      assert (d0.f22 = d2.f22);
      assert (d0.f23 = d2.f23);
      assert (d0.f24 = d2.f24);
      assert (d0.f25 = d2.f25);
      assert (d0.f26 = d2.f26);
      assert (d0.f27 = d2.f27);
      assert (d0.f28 = d2.f28);
      assert (d0.f29 = d2.f29);
      assert (d0.f30 = d2.f30);
      assert (d0.f31 = d2.f31);
      assert (d0.f32 = d2.f32);
      assert (d0.f33 = d2.f33);
      assert (d0.f34 = d2.f34);
      assert (d0.f35 = d2.f35);
      assert (d0.f0 = d3.f0);
      assert (d0.f1 = d3.f1);
      assert (d0.f2 = d3.f2);
      assert (d0.f3 = d3.f3);
      assert (d0.f4 = d3.f4);
      assert (d0.f5 = d3.f5);
      assert (d0.f6 = d3.f6);
      assert (d0.f7 = d3.f7);
      assert (d0.f8 = d3.f8);
      assert (d0.f9 = d3.f9);
      assert (d0.f10 = d3.f10);
      assert (d0.f11 = d3.f11);
      assert (d0.f12 = d3.f12);
      assert (d0.f13 = d3.f13);
      assert (d0.f14 = d3.f14);
      assert (d0.f15 = d3.f15);
      assert (d0.f16 = d3.f16);
      assert (d0.f17 = d3.f17);
      assert (d0.f18 = d3.f18);
      assert (d0.f19 = d3.f19);
      assert (d0.f20 = d3.f20);
      assert (d0.f21 = d3.f21);
      assert (d0.f22 = d3.f22);
      assert (d0.f23 = d3.f23);
      assert (d0.f24 = d3.f24);
      assert (d0.f25 = d3.f25);
      assert (d0.f26 = d3.f26);
      assert (d0.f27 = d3.f27);
      assert (d0.f28 = d3.f28);
      assert (d0.f29 = d3.f29);
      assert (d0.f30 = d3.f30);
      assert (d0.f31 = d3.f31);
      assert (d0.f32 = d3.f32);
      assert (d0.f33 = d3.f33);
      assert (d0.f34 = d3.f34);
      assert (d0.f35 = d3.f35);
      assert (d1.f0 = d8.f0);
      assert (d1.f1 = d8.f1);
      assert (d1.f2 = d8.f2);
      assert (d1.f3 = d8.f3);
      assert (d1.f4 = d8.f4);
      assert (d1.f5 = d8.f5);
      assert (d1.f6 = d8.f6);
      assert (d1.f7 = d8.f7);
      assert (d1.f8 = d8.f8);
      assert (d0.f9 = d8.f9);
      assert (d0.f10 = d8.f10);
      assert (d0.f11 = d8.f11);
      assert (d0.f12 = d8.f12);
      assert (d0.f13 = d8.f13);
      assert (d0.f14 = d8.f14);
      assert (d0.f15 = d8.f15);
      assert (d0.f16 = d8.f16);
      assert (d0.f17 = d8.f17);
      assert (d0.f18 = d8.f18);
      assert (d0.f19 = d8.f19);
      assert (d0.f20 = d8.f20);
      assert (d0.f21 = d8.f21);
      assert (d0.f22 = d8.f22);
      assert (d0.f23 = d8.f23);
      assert (d0.f24 = d8.f24);
      assert (d0.f25 = d8.f25);
      assert (d0.f26 = d8.f26);
      assert (d0.f27 = d8.f27);
      assert (d0.f28 = d8.f28);
      assert (d0.f29 = d8.f29);
      assert (d0.f30 = d8.f30);
      assert (d0.f31 = d8.f31);
      assert (d0.f32 = d8.f32);
      assert (d0.f33 = d8.f33);
      assert (d0.f34 = d8.f34);
      assert (d0.f35 = d8.f35);
      assert (d0.f0 = d9.f0);
      assert (d0.f1 = d9.f1);
      assert (d0.f2 = d9.f2);
      assert (d0.f3 = d9.f3);
      assert (d0.f4 = d9.f4);
      assert (d0.f5 = d9.f5);
      assert (d0.f6 = d9.f6);
      assert (d0.f7 = d9.f7);
      assert (d0.f8 = d9.f8);
      assert (d0.f9 = d9.f9);
      assert (d0.f10 = d9.f10);
      assert (d0.f11 = d9.f11);
      assert (d0.f12 = d9.f12);
      assert (d0.f13 = d9.f13);
      assert (d0.f14 = d9.f14);
      assert (d0.f15 = d9.f15);
      assert (d0.f16 = d9.f16);
      assert (d0.f17 = d9.f17);
      assert (d0.f18 = d9.f18);
      assert (d0.f19 = d9.f19);
      assert (d0.f20 = d9.f20);
      assert (d0.f21 = d9.f21);
      assert (d0.f22 = d9.f22);
      assert (d0.f23 = d9.f23);
      assert (d0.f24 = d9.f24);
      assert (d0.f25 = d9.f25);
      assert (d0.f26 = d9.f26);
      assert (d0.f27 = d9.f27);
      assert (d0.f28 = d9.f28);
      assert (d0.f29 = d9.f29);
      assert (d0.f30 = d9.f30);
      assert (d0.f31 = d9.f31);
      assert (d0.f32 = d9.f32);
      assert (d0.f33 = d9.f33);
      assert (d0.f34 = d9.f34);
      assert (d0.f35 = d9.f35);
      loop (n - 1)
  in
  loop 1_000_000
