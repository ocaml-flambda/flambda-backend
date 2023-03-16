; Generated automatically by this directory's dune.
; Run inc.sh to generate a new .inc file.

(alias (name regen))

 (rule
 (alias runtest)
  (deps tests13.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests13.fl))))

 (rule
 (alias runtest)
  (deps rec.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe rec.fl))))

 (rule
 (alias runtest)
  (deps size.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe size.fl))))

 (rule
 (alias runtest)
  (deps tests13_simple3.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests13_simple3.fl))))

 (rule
 (alias runtest)
  (deps arith.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe arith.fl))))

 (rule
 (alias runtest)
  (deps lengthapply.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe lengthapply.fl))))

 (rule
 (alias runtest)
  (deps noinline.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe noinline.fl))))

 (rule
 (alias runtest)
  (deps tests11_simple.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests11_simple.fl))))

 (rule
 (alias runtest)
  (deps anon.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe anon.fl))))

 (rule
 (alias runtest)
  (deps tests13_simple2.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests13_simple2.fl))))

 (rule
 (alias runtest)
  (deps tests12.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests12.fl))))

 (rule
 (alias runtest)
  (deps tests11.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests11.fl))))

 (rule
 (alias runtest)
  (deps apply.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe apply.fl))))

 (rule
 (alias runtest)
  (deps apply1.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe apply1.fl))))

 (rule
 (alias runtest)
  (deps apply2.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe apply2.fl))))

 (rule
 (alias runtest)
  (deps apply4.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe apply4.fl))))

 (rule
 (alias runtest)
  (deps apply5.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe apply5.fl))))

 (rule
 (alias runtest)
  (deps apply_noinline.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe apply_noinline.fl))))

 (rule
 (alias runtest)
  (deps apply_orig.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe apply_orig.fl))))

 (rule
 (alias runtest)
  (deps length.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe length.fl))))

 (rule
 (alias runtest)
  (deps tests13_simple.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests13_simple.fl))))

 (rule
 (alias runtest)
  (deps tests13_simpler.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe tests13_simpler.fl))))

 (rule
 (alias runtest)
  (deps opaque_partial.fl)
  (action
   (progn
   (run ../tools/flvalidate.exe opaque_partial.fl))))
