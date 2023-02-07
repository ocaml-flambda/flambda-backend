(
    .unix.objs/native/unix.cmx
    .unix.objs/native/unixLabels.cmx
    unix.cmxa
    unix.a
    unix.cma
    .unix.objs/byte/unix.cmi
    .unix.objs/byte/unix.cmti
    .unix.objs/byte/unixLabels.cmi
    .unix.objs/byte/unixLabels.cmti
    unix.cmxs
    unix.mli
    unixLabels.mli
    libunix_stubs.a
    (libunix_stubs.a as libunix_stubs_native.a) ; for special_dune compat
    (dllunix_stubs.so as stublibs/dllunix_stubs.so)
    (socketaddr.h as caml/socketaddr.h)
)
