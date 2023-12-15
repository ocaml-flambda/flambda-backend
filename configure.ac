AC_PREREQ([2.69])
AC_INIT([The Flambda backend for OCaml],
        5.1.1+jst,
        [mshinwell@janestreet.com],
        [flambda_backend],
        [http://github.com/ocaml-flambda/flambda_backend])

AC_MSG_NOTICE([Configuring Flambda backend version AC_PACKAGE_VERSION])

m4_include([autoconf-aux/ax_subdirs_configure.m4])

AC_CONFIG_AUX_DIR([autoconf-aux])

AC_PATH_PROG([dune], [dune], [])

AC_ARG_WITH([dune],
  [AS_HELP_STRING([--with-dune],
    [Path to dune executable (otherwise PATH is searched)])],
  [dune=$with_dune])

dnl Bug here somewhere -- if dune is on the path and an invalid --with-dune is
dnl specified, then we should stop.

AS_IF([test x"$dune" = "x"],
  [AC_MSG_ERROR([dune not found on PATH; install, or use --with-dune])])

AC_MSG_NOTICE([Using dune executable: $dune])

AC_ARG_ENABLE([middle-end],
  [AS_HELP_STRING([--enable-middle-end],
    [Select which middle end to use: closure, flambda or flambda2])],
  [AS_CASE([$enable_middle_end],
    [closure], [middle_end=closure middle_end_arg=--disable-flambda],
    [flambda], [middle_end=flambda middle_end_arg=--enable-flambda],
    [flambda2], [middle_end=flambda2 middle_end_arg=--enable-flambda2],
    [*], [AC_MSG_ERROR([Bad middle end (not closure, flambda or flambda2)])])],
  [AC_MSG_ERROR([--enable-middle-end=closure|flambda|flambda2 must be provided])])

AC_ARG_ENABLE([runtime5],
  [AS_HELP_STRING([--enable-runtime5],
    [Use the OCaml 5 runtime])],
  [runtime5_arg=--enable-runtime5],
  [runtime5_arg=])

AC_ARG_ENABLE([coverage],
  [AS_HELP_STRING([--enable-coverage],
    [Run compiler tests instrumented to output coverage data using bisect_ppx
     (WARNING: Cannot build an installable compiler with this flag enabled.
     Mainly intended for use in CI.)])],
  [coverage=yes],
  [coverage=no])

AC_ARG_ENABLE([legacy-library-layout],
  [AS_HELP_STRING([--enable-legacy-library-layout],
    [Install libraries unix, str, dynlink and bigarray in the toplevel
     ocaml library directory (Same as upstream OCaml < 5.0)])],
  [legacy_layout=yes],
  [legacy_layout=no])

AC_SUBST([prefix])
AC_SUBST([middle_end])
AC_SUBST([dune])
AC_SUBST([coverage])
AC_SUBST([legacy_layout])

# Don't error on options that this configure script doesn't understand but
# the ocaml/ one does.
AC_DISABLE_OPTION_CHECKING

AX_SUBDIRS_CONFIGURE([ocaml],
  [$middle_end_arg,$runtime5_arg,-C,--disable-ocamldoc,--disable-stdlib-manpages,--enable-ocamltest,--without-zstd],
  [],
  [],
  [])

AC_CONFIG_FILES([Makefile.config])
AC_OUTPUT
