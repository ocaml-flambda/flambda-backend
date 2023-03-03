; Generated automatically by this directory's dune.
; Use `make regen-flambda2-test-dune-rules` to regenerate.

(alias (name regen))

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
