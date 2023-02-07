(
    .str.objs/native/str.cmx
    libstr_stubs.a
    (libstr_stubs.a as libstr_stubs_native.a) ; for special_dune compat
    (dllstr_stubs.so as stublibs/dllstr_stubs.so)
    str.cmxa
    str.a
    str.cmxs
    str.cma
    str.mli
    .str.objs/byte/str.cmi
    .str.objs/byte/str.cmti

)
