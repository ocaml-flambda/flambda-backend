; Generated automatically by this directory's dune.
; Use `make regen-flambda2-test-dune-rules` to regenerate.

(alias (name regen))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe inlined_rec.flt)
   (diff? inlined_rec.flt inlined_rec.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe inlined_rec_in.fl)
   (diff? inlined_rec_in.fl inlined_rec_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe inlined_rec_out.fl)
   (diff? inlined_rec_out.fl inlined_rec_out.fl.corrected))))

(rule
 (alias regen)
 (targets inlined_rec.flt.new inlined_rec_in.fl.new inlined_rec_out.fl.new)
 (deps inlined_rec.ml)
 (action
   (run ocamlopt -c inlined_rec.ml -nopervasives -nostdlib
        -drawfexpr-to inlined_rec_in.fl.new
        -dfexpr-to inlined_rec_out.fl.new
        -dflexpect-to inlined_rec.flt.new)))

(rule
 (alias regen)
 (action (diff inlined_rec_in.fl inlined_rec_in.fl.new)))

(rule
 (alias regen)
 (action (diff inlined_rec_out.fl inlined_rec_out.fl.new)))

(rule
 (alias regen)
 (action (diff inlined_rec.flt inlined_rec.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe local.flt)
   (diff? local.flt local.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe local_in.fl)
   (diff? local_in.fl local_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe local_out.fl)
   (diff? local_out.fl local_out.fl.corrected))))

(rule
 (alias regen)
 (targets local.flt.new local_in.fl.new local_out.fl.new)
 (deps local.ml)
 (action
   (run ocamlopt -c local.ml -nopervasives -nostdlib
        -drawfexpr-to local_in.fl.new
        -dfexpr-to local_out.fl.new
        -dflexpect-to local.flt.new)))

(rule
 (alias regen)
 (action (diff local_in.fl local_in.fl.new)))

(rule
 (alias regen)
 (action (diff local_out.fl local_out.fl.new)))

(rule
 (alias regen)
 (action (diff local.flt local.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests0.flt)
   (diff? tests0.flt tests0.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests0_in.fl)
   (diff? tests0_in.fl tests0_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests0_out.fl)
   (diff? tests0_out.fl tests0_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests0.flt.new tests0_in.fl.new tests0_out.fl.new)
 (deps tests0.ml)
 (action
   (run ocamlopt -c tests0.ml -nopervasives -nostdlib
        -drawfexpr-to tests0_in.fl.new
        -dfexpr-to tests0_out.fl.new
        -dflexpect-to tests0.flt.new)))

(rule
 (alias regen)
 (action (diff tests0_in.fl tests0_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests0_out.fl tests0_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests0.flt tests0.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests2.flt)
   (diff? tests2.flt tests2.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests2_in.fl)
   (diff? tests2_in.fl tests2_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests2_out.fl)
   (diff? tests2_out.fl tests2_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests2.flt.new tests2_in.fl.new tests2_out.fl.new)
 (deps tests2.ml)
 (action
   (run ocamlopt -c tests2.ml -nopervasives -nostdlib
        -drawfexpr-to tests2_in.fl.new
        -dfexpr-to tests2_out.fl.new
        -dflexpect-to tests2.flt.new)))

(rule
 (alias regen)
 (action (diff tests2_in.fl tests2_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests2_out.fl tests2_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests2.flt tests2.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests3.flt)
   (diff? tests3.flt tests3.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests3_in.fl)
   (diff? tests3_in.fl tests3_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests3_out.fl)
   (diff? tests3_out.fl tests3_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests3.flt.new tests3_in.fl.new tests3_out.fl.new)
 (deps tests3.ml)
 (action
   (run ocamlopt -c tests3.ml -nopervasives -nostdlib
        -drawfexpr-to tests3_in.fl.new
        -dfexpr-to tests3_out.fl.new
        -dflexpect-to tests3.flt.new)))

(rule
 (alias regen)
 (action (diff tests3_in.fl tests3_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests3_out.fl tests3_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests3.flt tests3.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests4.flt)
   (diff? tests4.flt tests4.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4_in.fl)
   (diff? tests4_in.fl tests4_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4_out.fl)
   (diff? tests4_out.fl tests4_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests4.flt.new tests4_in.fl.new tests4_out.fl.new)
 (deps tests4.ml)
 (action
   (run ocamlopt -c tests4.ml -nopervasives -nostdlib
        -drawfexpr-to tests4_in.fl.new
        -dfexpr-to tests4_out.fl.new
        -dflexpect-to tests4.flt.new)))

(rule
 (alias regen)
 (action (diff tests4_in.fl tests4_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests4_out.fl tests4_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests4.flt tests4.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests4a.flt)
   (diff? tests4a.flt tests4a.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4a_in.fl)
   (diff? tests4a_in.fl tests4a_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4a_out.fl)
   (diff? tests4a_out.fl tests4a_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests4a.flt.new tests4a_in.fl.new tests4a_out.fl.new)
 (deps tests4a.ml)
 (action
   (run ocamlopt -c tests4a.ml -nopervasives -nostdlib
        -drawfexpr-to tests4a_in.fl.new
        -dfexpr-to tests4a_out.fl.new
        -dflexpect-to tests4a.flt.new)))

(rule
 (alias regen)
 (action (diff tests4a_in.fl tests4a_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests4a_out.fl tests4a_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests4a.flt tests4a.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests5.flt)
   (diff? tests5.flt tests5.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests5_in.fl)
   (diff? tests5_in.fl tests5_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests5_out.fl)
   (diff? tests5_out.fl tests5_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests5.flt.new tests5_in.fl.new tests5_out.fl.new)
 (deps tests5.ml)
 (action
   (run ocamlopt -c tests5.ml -nopervasives -nostdlib
        -drawfexpr-to tests5_in.fl.new
        -dfexpr-to tests5_out.fl.new
        -dflexpect-to tests5.flt.new)))

(rule
 (alias regen)
 (action (diff tests5_in.fl tests5_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests5_out.fl tests5_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests5.flt tests5.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests6.flt)
   (diff? tests6.flt tests6.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests6_in.fl)
   (diff? tests6_in.fl tests6_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests6_out.fl)
   (diff? tests6_out.fl tests6_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests6.flt.new tests6_in.fl.new tests6_out.fl.new)
 (deps tests6.ml)
 (action
   (run ocamlopt -c tests6.ml -nopervasives -nostdlib
        -drawfexpr-to tests6_in.fl.new
        -dfexpr-to tests6_out.fl.new
        -dflexpect-to tests6.flt.new)))

(rule
 (alias regen)
 (action (diff tests6_in.fl tests6_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests6_out.fl tests6_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests6.flt tests6.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests7.flt)
   (diff? tests7.flt tests7.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests7_in.fl)
   (diff? tests7_in.fl tests7_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests7_out.fl)
   (diff? tests7_out.fl tests7_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests7.flt.new tests7_in.fl.new tests7_out.fl.new)
 (deps tests7.ml)
 (action
   (run ocamlopt -c tests7.ml -nopervasives -nostdlib
        -drawfexpr-to tests7_in.fl.new
        -dfexpr-to tests7_out.fl.new
        -dflexpect-to tests7.flt.new)))

(rule
 (alias regen)
 (action (diff tests7_in.fl tests7_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests7_out.fl tests7_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests7.flt tests7.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests11.flt)
   (diff? tests11.flt tests11.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests11_in.fl)
   (diff? tests11_in.fl tests11_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests11_out.fl)
   (diff? tests11_out.fl tests11_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests11.flt.new tests11_in.fl.new tests11_out.fl.new)
 (deps tests11.ml)
 (action
   (run ocamlopt -c tests11.ml -nopervasives -nostdlib
        -drawfexpr-to tests11_in.fl.new
        -dfexpr-to tests11_out.fl.new
        -dflexpect-to tests11.flt.new)))

(rule
 (alias regen)
 (action (diff tests11_in.fl tests11_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests11_out.fl tests11_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests11.flt tests11.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests14.flt)
   (diff? tests14.flt tests14.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests14_in.fl)
   (diff? tests14_in.fl tests14_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests14_out.fl)
   (diff? tests14_out.fl tests14_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests14.flt.new tests14_in.fl.new tests14_out.fl.new)
 (deps tests14.ml)
 (action
   (run ocamlopt -c tests14.ml -nopervasives -nostdlib
        -drawfexpr-to tests14_in.fl.new
        -dfexpr-to tests14_out.fl.new
        -dflexpect-to tests14.flt.new)))

(rule
 (alias regen)
 (action (diff tests14_in.fl tests14_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests14_out.fl tests14_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests14.flt tests14.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests15.flt)
   (diff? tests15.flt tests15.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests15_in.fl)
   (diff? tests15_in.fl tests15_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests15_out.fl)
   (diff? tests15_out.fl tests15_out.fl.corrected))))

(rule
 (alias regen)
 (targets tests15.flt.new tests15_in.fl.new tests15_out.fl.new)
 (deps tests15.ml)
 (action
   (run ocamlopt -c tests15.ml -nopervasives -nostdlib
        -drawfexpr-to tests15_in.fl.new
        -dfexpr-to tests15_out.fl.new
        -dflexpect-to tests15.flt.new)))

(rule
 (alias regen)
 (action (diff tests15_in.fl tests15_in.fl.new)))

(rule
 (alias regen)
 (action (diff tests15_out.fl tests15_out.fl.new)))

(rule
 (alias regen)
 (action (diff tests15.flt tests15.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll.flt)
   (diff? unroll.flt unroll.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll_in.fl)
   (diff? unroll_in.fl unroll_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll_out.fl)
   (diff? unroll_out.fl unroll_out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll.flt.new unroll_in.fl.new unroll_out.fl.new)
 (deps unroll.ml)
 (action
   (run ocamlopt -c unroll.ml -nopervasives -nostdlib
        -drawfexpr-to unroll_in.fl.new
        -dfexpr-to unroll_out.fl.new
        -dflexpect-to unroll.flt.new)))

(rule
 (alias regen)
 (action (diff unroll_in.fl unroll_in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll_out.fl unroll_out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll.flt unroll.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll2.flt)
   (diff? unroll2.flt unroll2.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll2_in.fl)
   (diff? unroll2_in.fl unroll2_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll2_out.fl)
   (diff? unroll2_out.fl unroll2_out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll2.flt.new unroll2_in.fl.new unroll2_out.fl.new)
 (deps unroll2.ml)
 (action
   (run ocamlopt -c unroll2.ml -nopervasives -nostdlib
        -drawfexpr-to unroll2_in.fl.new
        -dfexpr-to unroll2_out.fl.new
        -dflexpect-to unroll2.flt.new)))

(rule
 (alias regen)
 (action (diff unroll2_in.fl unroll2_in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll2_out.fl unroll2_out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll2.flt unroll2.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll3.flt)
   (diff? unroll3.flt unroll3.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll3_in.fl)
   (diff? unroll3_in.fl unroll3_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll3_out.fl)
   (diff? unroll3_out.fl unroll3_out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll3.flt.new unroll3_in.fl.new unroll3_out.fl.new)
 (deps unroll3.ml)
 (action
   (run ocamlopt -c unroll3.ml -nopervasives -nostdlib
        -drawfexpr-to unroll3_in.fl.new
        -dfexpr-to unroll3_out.fl.new
        -dflexpect-to unroll3.flt.new)))

(rule
 (alias regen)
 (action (diff unroll3_in.fl unroll3_in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll3_out.fl unroll3_out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll3.flt unroll3.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll4.flt)
   (diff? unroll4.flt unroll4.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll4_in.fl)
   (diff? unroll4_in.fl unroll4_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll4_out.fl)
   (diff? unroll4_out.fl unroll4_out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll4.flt.new unroll4_in.fl.new unroll4_out.fl.new)
 (deps unroll4.ml)
 (action
   (run ocamlopt -c unroll4.ml -nopervasives -nostdlib
        -drawfexpr-to unroll4_in.fl.new
        -dfexpr-to unroll4_out.fl.new
        -dflexpect-to unroll4.flt.new)))

(rule
 (alias regen)
 (action (diff unroll4_in.fl unroll4_in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll4_out.fl unroll4_out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll4.flt unroll4.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll5.flt)
   (diff? unroll5.flt unroll5.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll5_in.fl)
   (diff? unroll5_in.fl unroll5_in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll5_out.fl)
   (diff? unroll5_out.fl unroll5_out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll5.flt.new unroll5_in.fl.new unroll5_out.fl.new)
 (deps unroll5.ml)
 (action
   (run ocamlopt -c unroll5.ml -nopervasives -nostdlib
        -drawfexpr-to unroll5_in.fl.new
        -dfexpr-to unroll5_out.fl.new
        -dflexpect-to unroll5.flt.new)))

(rule
 (alias regen)
 (action (diff unroll5_in.fl unroll5_in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll5_out.fl unroll5_out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll5.flt unroll5.flt.new)))
