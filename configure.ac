AC_PREREQ([2.69])
AC_INIT([The Flambda backend for OCaml],
        4.11.1+dev0-2020-08-18,
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
    [Select which middle end to use: closure or flambda])],
  [AS_CASE([$enable_middle_end],
    [closure], [middle_end=closure middle_end_arg=--disable-flambda],
    [flambda], [middle_end=flambda middle_end_arg=--enable-flambda],
    [*],       [AC_MSG_ERROR([Bad middle end (not closure or flambda)])])],
  [AC_MSG_ERROR([--enable-middle-end=closure|flambda must be provided])])

stage0_prefix=$ac_abs_confdir/_build0/_install
stage1_prefix=$ac_abs_confdir/_build1/install/default
stage2_prefix=$ac_abs_confdir/_build2/install/default

AC_SUBST([prefix])
AC_SUBST([middle_end])
AC_SUBST([stage0_prefix])
AC_SUBST([stage1_prefix])
AC_SUBST([stage2_prefix])
AC_SUBST([dune])

# Don't error on options that this configure script doesn't understand but
# the ocaml/ one does.
AC_DISABLE_OPTION_CHECKING

# The "real" --prefix will still be on the command line, but our new one here
# will take priority.
# This should match configure_args_for_ocaml, below.
AX_SUBDIRS_CONFIGURE([ocaml],
  [$middle_end_arg,-C,--disable-ocamldoc],
  [],
  [--prefix=$stage1_prefix],
  [])

temp=$(mktemp)

echo "$middle_end_arg" > $temp
echo "--disable-ocamldoc" >> $temp

# The following trick ensures that if one of the arguments to configure
# contains a space, the argument is not incorrectly split on such space.
eval 'for option in '$ac_configure_args'; do echo $option; done' \
  | grep -v '^--prefix=' \
  | grep -v '^--enable-middle-end=' \
  | grep -v '^--with-dune=' \
  >> $temp

# We prefer nulls to newlines as delimiters as it makes it easier to
# use xargs across platforms (macOS doesn't support xargs -d'\n' for
# example).
cat $temp | tr '\n' '\0' > configure_opts

rm -f $temp

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

# We need a copy of config.status that is, for certain, the correct one
# for the stage1 build.
cp ocaml/config.status ocaml-stage1-config.status
