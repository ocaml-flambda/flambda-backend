AC_PREREQ([2.69])
AC_INIT([The Flambda backend for OCaml],
        4.11-0,
        [mshinwell@janestreet.com],
        [flambda_backend],
        [http://github.com/ocaml-flambda/flambda_backend])

AC_MSG_NOTICE([Configuring Flambda backend version AC_PACKAGE_VERSION])

m4_include([autoconf-aux/ax_subdirs_configure.m4])

AC_CONFIG_AUX_DIR([autoconf-aux])

AC_CHECK_PROG([ocamlopt], [ocamlopt], [true], [false])

AS_IF([test x"$ocamlopt" = "xfalse"],
  [AC_MSG_ERROR([ocamlopt not found on PATH, please select 4.11 OPAM switch])])

AC_MSG_NOTICE([ocamlopt found ok on PATH])

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
    [Select which middle end to use: closure or flambda])],
  [AS_CASE([$enable_middle_end],
    [closure], [middle_end=closure middle_end_arg=--disable-flambda],
    [flambda], [middle_end=flambda middle_end_arg=--enable-flambda],
    [*],       [AC_MSG_ERROR([Bad middle end (not closure or flambda)])])],
  [AC_MSG_ERROR([--enable-middle-end=closure|flambda must be provided])])

stage1_prefix=$ac_abs_confdir/_build1/install/default
stage2_prefix=$ac_abs_confdir/_build2/install/default

AC_SUBST([prefix])
AC_SUBST([middle_end])
AC_SUBST([stage1_prefix])
AC_SUBST([stage2_prefix])
AC_SUBST([dune])

# Don't error on options that this configure script doesn't understand but
# the ocaml/ one does.
AC_DISABLE_OPTION_CHECKING

# The "real" --prefix will still be on the command line, but our new one here
# will take priority.
AX_SUBDIRS_CONFIGURE([ocaml],
  [$middle_end_arg,-C],
  [],
  [--prefix=$stage1_prefix],
  [])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

# We need a copy of config.status that is, for certain, the correct one
# for the stage1 build.
cp ocaml/config.status ocaml-stage1-config.status
