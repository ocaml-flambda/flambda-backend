AC_PREREQ([2.69])
AC_INIT([The Flambda backend for OCaml],
        5.1.1+jst,
        [mshinwell@janestreet.com],
        [flambda_backend],
        [http://github.com/ocaml-flambda/flambda_backend])

DUNE_MAX_VERSION=[3.8]

AC_MSG_NOTICE([Configuring Flambda backend version AC_PACKAGE_VERSION])

m4_include([autoconf-aux/ax_subdirs_configure.m4])
m4_include([autoconf-aux/ax_compare_version.m4])

AC_CONFIG_AUX_DIR([autoconf-aux])

AC_PATH_PROG([dune], [dune], [])

AC_ARG_WITH([dune],
  [AS_HELP_STRING([--with-dune],
    [Path to dune executable (otherwise PATH is searched)])],
  [dune=$with_dune])

AS_IF([test x"$dune" = "x"],
  [AC_MSG_ERROR([dune not found on PATH; install, or use --with-dune])])

dune_version=`$dune --version | sed -e 's/^\([[0-9]]\.[[0-9]]\).*/\1/'`

AS_IF([test x"$dune_version" = "x"],
  [AC_MSG_ERROR([unable to execute dune at $dune])])

dnl If dune is too new, keep going but warn at the end
AX_COMPARE_VERSION([$dune_version], [le], [$DUNE_MAX_VERSION],
  [dune_version_good=true],
  [dune_version_good=false])

AC_MSG_NOTICE([Using dune executable: $dune])

AC_ARG_ENABLE([middle-end],
  [AS_HELP_STRING([--enable-middle-end],
    [Select which middle end to use: flambda2 (default) or upstream-closure])],
  [AS_CASE([$enable_middle_end],
    [upstream-closure], [middle_end=closure middle_end_arg=--disable-flambda],
    [flambda2], [middle_end=flambda2 middle_end_arg=--enable-flambda2],
    [*], [AC_MSG_ERROR([Invalid middle end: must be flambda2 or upstream-closure])])],
  [middle_end=flambda2 middle_end_arg=--enable-flambda2])

AC_ARG_ENABLE([runtime5],
  [AS_HELP_STRING([--enable-runtime5],
    [Use the OCaml 5 runtime])],
  [],
  [enable_runtime5=no])

AC_ARG_ENABLE([coverage],
  [AS_HELP_STRING([--enable-coverage],
    [Run compiler tests instrumented to output coverage data using bisect_ppx
     (WARNING: Cannot build an installable compiler with this flag enabled.
     Mainly intended for use in CI.)])],
  [],
  [enable_coverage=no])

AC_ARG_ENABLE([legacy-library-layout],
  [AS_HELP_STRING([--enable-legacy-library-layout],
    [Install libraries unix, str, dynlink and bigarray in the toplevel
     ocaml library directory (Same as upstream OCaml < 5.0)])],
  [],
  [enable_legacy_layout=no])

AC_ARG_ENABLE([dev],
  [AS_HELP_STRING([--enable-dev],
    [Use the dev build profile for dune when building the 2nd stage.
     Can speed up the compile time by a lot. (WARNING: do not use for
     production compiler deployments)])],
  [],
  [enable_dev=no])

AC_SUBST([prefix])
AC_SUBST([middle_end])
AC_SUBST([dune])

coverage="$enable_coverage"
AC_SUBST([coverage])

legacy_layout="$enable_legacy_layout"
AC_SUBST([legacy_layout])

main_build_profile=main
AS_IF([test x"$enable_dev" = xyes], [main_build_profile=dev])
AC_SUBST([main_build_profile])

# Don't error on options that this configure script doesn't understand but
# the ocaml/ one does.
AC_DISABLE_OPTION_CHECKING

AX_SUBDIRS_CONFIGURE([ocaml],
  [$middle_end_arg,--enable-runtime5=$enable_runtime5,-C,--disable-ocamldoc,--disable-stdlib-manpages,--enable-ocamltest,--without-zstd],
  [],
  [],
  [])

AC_CONFIG_FILES([Makefile.config])
AC_OUTPUT

AS_IF([test x"$dune_version_good" = "xfalse"],
  [AC_MSG_WARN([dune $dune_version found; only dune up to $DUNE_MAX_VERSION known to work])])
