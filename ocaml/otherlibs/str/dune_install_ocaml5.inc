(
   (.str.objs/native/str.cmx as str/str.cmx)
    (libstr_stubs.a as str/libstr_stubs.a)
    (libstr_stubs.a as libstr_stubs_native.a) ; for special_dune compat
    (dllstr_stubs.so as stublibs/dllstr_stubs.so)
    (str.cmxa as str/str.cmxa)
    (str.a as str/str.a)
    (str.cmxs as str/str.cmxs)
    (str.cma as str/str.cma)
    (str.mli as str/str.mli)
    (.str.objs/byte/str.cmi as str/str.cmi)
    (.str.objs/byte/str.cmti as str/str.cmti)
)
