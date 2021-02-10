#!/bin/bash

# Compare the installation tree produced by the Flambda backend build
# process against that from an upstream compiler built with make.

set -eu -o pipefail

if [ "$(uname)" != "Linux" ]; then
  echo "This script ($0) should be run on a Linux machine."
  exit 1
fi

if [ $# != 3 ]; then
  echo "syntax: $0 UPSTREAM-INSTALL-DIR FLAMBDA-BACKEND-INSTALL-DIR OCAMLOBJINFO"
  exit 1
fi

difftool="patdiff"
if ! which patdiff > /dev/null 2>&1 ; then
  echo "Warning: patdiff not found, falling back to diff"
  echo
  difftool="diff -U3"
fi

# Installation root (--prefix) for the upstream and Flambda backend compilers
# We'd like these not to have trailing slashes as they are used for matching
# below.
upstream_tree=$(echo $1 | sed 's:/$::')
flambda_backend_tree=$(echo $2 | sed 's:/$::')
ocamlobjinfo=$3

if [ ! -x "$ocamlobjinfo" ]; then
  echo "Missing ocamlobjinfo, expected at: $ocamlobjinfo"
  exit 1
fi

# Avoid silent failure to detect installation of ocamlnat on the Flambda
# backend side.
if [ ! -x "$upstream_tree/bin/ocamlnat" ]; then
  echo "'make ocamlnat' was not run on the upstream tree"
  exit 1
fi

# These filenames are the ones from the Flambda backend install tree.
# ocamloptcomp.a will diverge in time, but it's ok to compare right now.

# It would be nice in the future to automatically determine the list of
# .a, .so, .cma and .cmxa files to compare.

archives_to_compare="\
  libasmrun.a \
  libasmrund.a \
  libasmruni.a \
  libasmrun_pic.a \
  libcamlrun.a \
  libcamlrund.a \
  libcamlruni.a \
  libcamlrun_pic.a \
  bigarray.a \
  compiler-libs/ocamlbytecomp.a \
  compiler-libs/ocamlcommon.a \
  stdlib.a \
  str.a \
  threads/threads.a \
  unix.a \
  libthreads_stubs.a \
  libthreads_stubs_native.a \
  libunix_stubs.a \
  libunix_stubs_native.a \
  libstr_stubs.a \
  libstr_stubs_native.a
  "

# CR mshinwell: Put this back in the list when we've got the ocaml/ dir
# resurrected properly at the correct rev.  (Or maybe not, since it will
# diverge and cause CI failures.)
#  compiler-libs/ocamloptcomp.a

# compiler-libs/ocamlopttoplevel.a has a discrepancy: Genprintval.Make is
# missing in favour of an extra anonymous function.  It is not clear why
# this is the case; the source is the same.  The toplevel is being refactored
# in any case, so it seems reasonable to defer until that happens.

# We don't currently check dynlink.a or the other Dynlink artifacts because the
# build process is quite different (and we currently have a source code patch in
# the Flambda backend to work around limitations of Dune, although we will
# try to upstream that).  We have execution tests for dynlink in the JS tree in
# any case.

# compiler-libs/ocamlmiddleend.a is not built in the Flambda backend.
# We should try to remove this from upstream by fixing the ocamlobjinfo
# problem.  Note that ocamloptcomp.a contains the middle end, both in
# the Flambda backend and upstream builds.

# These filenames are the ones from the Flambda backend install tree.
stublibs_to_compare="\
  dllstr_stubs.so \
  dllthreads_stubs.so \
  dllunix_stubs.so
  "

shared_objects_to_compare="\
  bigarray.cmxs \
  str.cmxs \
  unix.cmxs
  "

sundry_text_files_to_compare="\
  camlheader \
  camlheaderd \
  camlheaderi \
  camlheader_ur \
  eventlog_metadata \
  ld.conf
  "

cma_to_compare="\
  stdlib.cma \
  bigarray.cma \
  threads/threads.cma \
  compiler-libs/ocamlcommon.cma \
  compiler-libs/ocamlbytecomp.cma \
  compiler-libs/ocamltoplevel.cma \
  unix.cma \
  str.cma
  "
# No dynlink.cma as per comment above.
# Also no compiler-libs/ocamloptcomp.cma, see above.

cmxa_to_compare="\
  stdlib.cmxa \
  bigarray.cmxa \
  threads/threads.cmxa \
  compiler-libs/ocamlcommon.cmxa \
  compiler-libs/ocamlbytecomp.cmxa \
  unix.cmxa \
  str.cmxa
  "
# No dynlink.cmxa as per comment above.
# Also no compiler-libs/ocamloptcomp.cmxa, see above.

# compiler-libs/ocamlopttoplevel.cmxa has some almost certainly irrelevant
# discrepancies, although the flags match.  We can come back to this after
# the toplevel code has been refactored.

# Middle end and back end .cmx files which are excluded from comparison,
# since they will diverge.
cmx_exclusions="\
    reg.cmx \
    linscan.cmx \
    printlinear.cmx \
    CSEgen.cmx \
    selection.cmx \
    coloring.cmx \
    interval.cmx \
    schedgen.cmx \
    strmatch.cmx \
    CSE.cmx \
    reloadgen.cmx \
    cmm.cmx \
    asmgen.cmx \
    deadcode.cmx \
    reg_availability_set.cmx \
    available_regs.cmx \
    compute_ranges_intf.cmx \
    compute_ranges.cmx \
    reg_with_debug_info.cmx \
    liveness.cmx \
    x86_gas.cmx \
    linearize.cmx \
    proc.cmx \
    emit.cmx \
    cmmgen_state.cmx \
    interf.cmx \
    branch_relaxation_intf.cmx \
    arch.cmx \
    selectgen.cmx \
    afl_instrument.cmx \
    asmlibrarian.cmx \
    printmach.cmx \
    emitaux.cmx \
    asmlink.cmx \
    reload.cmx \
    x86_dsl.cmx \
    scheduling.cmx \
    split.cmx \
    x86_masm.cmx \
    comballoc.cmx \
    cmm_helpers.cmx \
    x86_proc.cmx \
    linear.cmx \
    branch_relaxation.cmx \
    spill.cmx \
    asmpackager.cmx \
    cmmgen.cmx \
    printcmm.cmx \
    mach.cmx \
    printclambda.cmx \
    ref_to_variables.cmx \
    parameter.cmx \
    traverse_for_exported_symbols.cmx \
    share_constants.cmx \
    simple_value_approx.cmx \
    export_info.cmx \
    remove_free_vars_equal_to_args.cmx \
    inconstant_idents.cmx \
    closure_conversion.cmx \
    closure_conversion_aux.cmx \
    extract_projections.cmx \
    flambda_invariants.cmx \
    inlining_stats_types.cmx \
    flambda_utils.cmx \
    inline_and_simplify_aux.cmx \
    export_info_for_pack.cmx \
    inline_and_simplify.cmx \
    inlining_transforms.cmx \
    flambda_to_clambda.cmx \
    alias_analysis.cmx \
    closure_offsets.cmx \
    lift_constants.cmx \
    flambda_middle_end.cmx \
    flambda.cmx \
    import_approx.cmx \
    augment_specialised_args.cmx \
    lift_code.cmx \
    inlining_cost.cmx \
    unbox_specialised_args.cmx \
    unbox_free_vars_of_closures.cmx \
    allocated_const.cmx \
    simplify_boxed_integer_ops.cmx \
    remove_unused_closure_vars.cmx \
    find_recursive_functions.cmx \
    inlining_decision.cmx \
    pass_wrapper.cmx \
    projection.cmx \
    closure_element.cmx \
    static_exception.cmx \
    tag.cmx \
    id_types.cmx \
    set_of_closures_origin.cmx \
    closure_id.cmx \
    mutable_variable.cmx \
    set_of_closures_id.cmx \
    export_id.cmx \
    var_within_closure.cmx \
    closure_origin.cmx \
    unbox_closures.cmx \
    simplify_primitives.cmx \
    freshening.cmx \
    flambda_iterators.cmx \
    build_export_info.cmx \
    remove_unused_arguments.cmx \
    invariant_params.cmx \
    lift_let_to_initialize_symbol.cmx \
    remove_unused_program_constructs.cmx \
    inlining_stats.cmx \
    initialize_symbol_to_let_symbol.cmx \
    effect_analysis.cmx \
    simplify_common.cmx \
    un_anf.cmx \
    compilenv.cmx \
    printclambda_primitives.cmx \
    clambda_primitives.cmx \
    symbol.cmx \
    closure_middle_end.cmx \
    closure.cmx \
    variable.cmx \
    compilation_unit.cmx \
    clambda.cmx \
    semantics_of_primitives.cmx \
    backend_var.cmx \
    convert_primitives.cmx \
    linkage_name.cmx \
    internal_variable_names.cmx \
  "

# Note that for the other installed artifacts for CSE and CSEgen the
# dune file in the Flambda backend gives them the correct names rather
# than the miscapitalised ones.
upstream_filename_of_archive_member () {
  filename=$1

  case "$filename" in
    cSEgen.o) echo CSEgen.o ;;
    cSE.o) echo CSE.o ;;
    st_stubs_byte.o) echo st_stubs_b.o ;;
    st_stubs_native.o) echo st_stubs_n.o ;;
    genprintval_native.o) echo genprintval.o ;;
    *) echo $filename ;;
  esac
}

upstream_filenames_of_archive_members () {
  # Dune doesn't follow the capitalisation convention of the compiler for
  # build artifacts.  This seems like a bug, but it should be harmless in
  # the following cases.
  while read ar_output; do
    filename=$(echo $ar_output | awk '{print $3}')
    upstream_filename_of_archive_member "$filename"
  done
}

upstream_filename_of_archive () {
  filename=$1

  case "$filename" in
    libthreads_stubs.a) echo libthreads.a ;;
    libthreads_stubs_native.a) echo libthreadsnat.a ;;
    libunix_stubs.a) echo libunix.a ;;
    libunix_stubs_native.a) echo libunix.a ;;
    libstr_stubs.a) echo libcamlstr.a ;;
    libstr_stubs_native.a) echo libcamlstr.a ;;
    *) echo $filename ;;
  esac
}

upstream_filename_of_stublibs () {
  filename=$1

  case "$filename" in
    dllstr_stubs.so) echo dllcamlstr.so ;;
    dllthreads_stubs.so) echo dllthreads.so ;;
    dllunix_stubs.so) echo dllunix.so ;;
    *) echo $filename ;;
  esac
}

ensure_exists () {
  file=$1

  if [ ! -f "$file" ]; then
    echo "File $file is missing"
    exit 1
  fi
}

list_object_file_symbols () {
  file=$1
  symbols=$2
  symbols_all=$3

  # We expect there may be some discrepancies in camlinternalMenhirLib,
  # but those should be harmless.

  # We currently ignore references to ceil () and nextafter () which in
  # the Unix bytecode stublibs seem to be referencing the non-versioned
  # symbols instead of the glibc-versioned symbols.  This is probably some
  # artifact of exactly how the libraries were produced but seems harmless.
  # (Might be due to an explicit provision of "-lm" to the linker, or
  # similar?)

  # Genprintval is named differently in ocamlopttoplevel.cmxa, we probably
  # don't need to do anything about that yet, as the toplevel code is
  # being refactored by Louis and Jeremie at present.

  nm $file \
    | sed 's/^...................//' \
    | grep -iv menhirlib \
    | grep -v '^camlDomainstate__' \
    | grep -v '^.LC[0-9]*$' \
    | grep -v '^.L[0-9]*$' \
    | sed 's/_[0-9]*$//' \
    | grep -v '^ceil@@GLIBC' \
    | grep -v '^ceil$' \
    | grep -v '^nextafter@@GLIBC' \
    | grep -v '^nextafter$' \
    | sed 's/camlGenprintval_native/camlGenprintval/' \
    > $symbols

  nm $file \
    | sed 's/^...................//' \
    | grep -iv menhirlib \
    | grep -v '^camlDomainstate__' \
    | grep -v '^ceil@@GLIBC' \
    | grep -v '^ceil$' \
    | grep -v '^nextafter@@GLIBC' \
    | grep -v '^nextafter$' \
    | sed 's/camlGenprintval_native/camlGenprintval/' \
    > $symbols_all
}

compare_object_file_symbols () {
  upstream_file=$1
  flambda_backend_file=$2

  upstream_symbols=$(mktemp)
  flambda_backend_symbols=$(mktemp)

  upstream_symbols_all=$(mktemp)
  flambda_backend_symbols_all=$(mktemp)

  list_object_file_symbols $upstream_file $upstream_symbols \
    $upstream_symbols_all
  list_object_file_symbols $flambda_backend_file $flambda_backend_symbols \
    $flambda_backend_symbols_all

  $difftool $upstream_symbols $flambda_backend_symbols \
    || (echo "Symbols do not match."; \
        rm -f $upstream_symbols $flambda_backend_symbols; \
        rm -f $upstream_symbols_all $flambda_backend_symbols_all; \
        exit 1)

  $difftool $upstream_symbols_all $flambda_backend_symbols_all || true

  rm -f $upstream_symbols $flambda_backend_symbols
  rm -f $upstream_symbols_all $flambda_backend_symbols_all
}

compare_archive () {
  archive=$1
  upstream_archive=$(upstream_filename_of_archive $archive)

  if [ "$archive" = "$upstream_archive" ]; then
    echo "Comparing archive: $archive"
  else
    echo "Comparing archive: $archive (upstream: $upstream_archive)"
  fi

  upstream=$(mktemp -d)
  flambda_backend=$(mktemp -d)

  upstream_contents=$(mktemp)
  flambda_backend_contents=$(mktemp)

  upstream_archive=$upstream_tree/lib/ocaml/$upstream_archive
  flambda_backend_archive=$flambda_backend_tree/lib/ocaml/$archive

  ensure_exists $upstream_archive
  ensure_exists $flambda_backend_archive

  cd $upstream \
    && ar xv $upstream_archive | sort > $upstream_contents
  cd $flambda_backend \
    && ar xv $flambda_backend_archive | sort > $flambda_backend_contents

  $difftool <(cat $upstream_contents | awk '{print $3}') \
    <(cat $flambda_backend_contents | upstream_filenames_of_archive_members) \
    || (echo "File names inside archive $archive do not match"; \
        rm -rf $upstream; \
        rm -rf $flambda_backend; \
        rm -f $upstream_contents; \
        rm -f $flambda_backend_contents; \
        exit 1)

  files=$(ls $flambda_backend)

  for file in $files; do
    upstream_file=$(upstream_filename_of_archive_member $file)

    if [ "$file" = "$upstream_file" ]; then
      echo "... Comparing symbols in $file"
    else
      echo "... Comparing symbols in $file (upstream $upstream_file)"
    fi

    compare_object_file_symbols $upstream/$upstream_file $flambda_backend/$file
  done

  rm -rf $upstream
  rm -rf $flambda_backend
  rm -f $upstream_contents
  rm -f $flambda_backend_contents
}

compare_stublibs_or_other_dot_so_or_cmxs () {
  stublibs=$1
  subdir=$2

  upstream_stublibs=$(upstream_filename_of_stublibs $stublibs)

  if [ "$stublibs" = "$upstream_stublibs" ]; then
    echo "Comparing stublibs or other .so / .cmxs: $stublibs"
  else
    echo -n "Comparing stublibs or other .so / .cmxs: $stublibs "
    echo "(upstream: $upstream_stublibs)"
  fi

  upstream_stublibs=$upstream_tree/lib/ocaml/$subdir/$upstream_stublibs
  flambda_backend_stublibs=$flambda_backend_tree/lib/ocaml/$subdir/$stublibs

  ensure_exists $upstream_stublibs
  ensure_exists $flambda_backend_stublibs

  compare_object_file_symbols $upstream_stublibs $flambda_backend_stublibs
}

compare_ml_and_mli_files () {
  dir=$1
  upstream_files=$(cd $upstream_tree/$dir && ls *.ml{,i} 2>/dev/null || true)
  for file in $upstream_files; do
    # We don't have Optmain in the Flambda backend at present (it's called
    # Flambda_backend_main instead).
    # Skip camlinternalMenhirLib until this warning 67 nonsense is sorted.
    if [ "$file" = "optmain.ml" ] || [ "$file" = "optmain.mli" ]; then
      echo "... skipping optmain.ml{,i}"
    elif [ "$file" = "camlinternalMenhirLib.ml" ]; then
      echo "... skipping camlinternalMenhirLib.ml"
    elif [ "$file" = "camlinternalMenhirLib.mli" ]; then
      echo "... skipping camlinternalMenhirLib.mli"
    else
      if [ ! -f "$flambda_backend_tree/$dir/$file" ]; then
        echo "$dir/$file is missing"
        exit 1
      fi

      # For some reason certain source files have location lines added of
      # the form:
      #   # 1 "ocaml/driver/compenv.mli"
      # which are harmless, so we filter them out.

      $difftool \
        <(cat $upstream_tree/$dir/$file | grep -v '^# [0-9]\+ \"') \
        <(cat $flambda_backend_tree/$dir/$file | grep -v '^# [0-9]\+ \"')
    fi
  done
}

remove_digests_from_objinfo_output () {
  # We don't currently require digests to match.

  # The replacement pattern isn't significant, but this makes the output
  # easy to inspect manually.
  sed -r 's/\t[a-f0-9]{32}\t/\t--------------------------------\t/'
}

header_of_objinfo_output () {
  grep -B 100 -m 1 "^Unit name: " | grep -v "^Unit name: " \
    | grep -v "^File "
}

sort_body_of_objinfo_output_bytecode () {
  # This ensures that the order of compilation units in the objinfo output
  # is consistent, even if it isn't in the file being examined (which
  # doesn't matter).

  objinfo_output=$1
  temp=$(mktemp -d)

  cat $objinfo_output \
    | grep -A 100000 -m 1 "^Unit name" \
    | while read line; do
        if [[ "$line" =~ ^Unit\ name:\ (.*)$ ]]; then
          # The wonders of dynamic scoping...
          compunit=${BASH_REMATCH[1]}
          echo > $temp/$compunit
          echo $line >> $temp/$compunit
        else
          echo $line >> $temp/$compunit
        fi
      done

  # We don't compare "required globals" since the use of -no-alias-deps
  # everywhere in Dune causes some discrepancies.
  for file in $temp/*; do
    cat $file \
      | grep -B 100000 -m 1 "^Required globals:" \
      > $file.finished

    cat $file \
      | grep -A 100000 -m 1 "^Uses unsafe features:" \
      >> $file.finished
  done

  # This expansion is guaranteed to be in a particular order.
  result=$(cat $temp/*.finished)

  echo "$temp" >> /tmp/log2

  rm -rf $temp

  echo "$result"
}

sort_body_of_objinfo_output_native () {
  objinfo_output=$1
  temp=$(mktemp -d)

  cat $objinfo_output \
    | grep -A 100000 -m 1 "^Name: " \
    | while read line; do
        if [[ "$line" =~ ^Name:\ (.*)$ ]]; then
          compunit=${BASH_REMATCH[1]}
          echo > $temp/$compunit
          echo $line >> $temp/$compunit
        else
          echo $line >> $temp/$compunit
        fi
      done

  # Prevent spurious errors from egrep
  cd $upstream_tree

  for file in $temp/*; do
    cat $file \
      | grep -B 100000 -m 1 "^Implementations imported:" \
      > $file.finished

    cat $file \
      | egrep -A 100000 -m 1 \
        "^(Clambda approximation:|Flambda export information:)" \
      >> $file.finished \
      || true
  done

  result=$(cat $temp/*.finished)

  rm -rf $temp

  echo "$result"
}

rewrite_flambda_backend_objinfo_c_library_names () {
  while read line; do
    key=$(echo $line | sed 's/: .*//')
    data=$(echo $line | sed 's/^[^:]*: //')
    case "$key" in
      "Extra C object files" | "Extra dynamically-loaded libraries")
        echo -n "$key: "
        for flag in $data; do
          if [[ "$flag" =~ ^-l(.*)$ ]]; then
            lib_name=${BASH_REMATCH[1]}
            archive_name=lib${lib_name}.a
            upstream_archive_name=$(upstream_filename_of_archive $archive_name)
            upstream_lib_name=$(echo $upstream_archive_name \
              | sed 's/\.a$//' \
              | sed 's/^lib//')
            echo -n "-l$upstream_lib_name"
          else
            echo -n $flag
          fi
          echo -n " "
        done
        echo
        ;;
      *) echo $line ;;
    esac
  done
}

compare_cma_files () {
  cma=$1

  echo "Comparing .cma file: $cma"

  upstream_cma=$upstream_tree/$cma
  flambda_backend_cma=$flambda_backend_tree/$cma

  ensure_exists $upstream_cma
  ensure_exists $flambda_backend_cma

  upstream=$(mktemp)
  flambda_backend=$(mktemp)

  $ocamlobjinfo $upstream_cma \
    | remove_digests_from_objinfo_output \
    > $upstream

  $ocamlobjinfo $flambda_backend_cma \
    | remove_digests_from_objinfo_output \
    | rewrite_flambda_backend_objinfo_c_library_names \
    > $flambda_backend

  $difftool <(cat $upstream | header_of_objinfo_output) \
    <(cat $flambda_backend | header_of_objinfo_output) \
    || (rm -f $upstream;
        rm -f $flambda_backend;
        exit 1
       )

  $difftool <(sort_body_of_objinfo_output_bytecode $upstream) \
    <(sort_body_of_objinfo_output_bytecode $flambda_backend) \
    || (rm -f $upstream;
        rm -f $flambda_backend;
        exit 1
       )

  rm -f $upstream
  rm -f $flambda_backend
}

compare_cmxa_files () {
  cmxa=$1

  echo "Comparing .cmxa file: $cmxa"

  upstream_cmxa=$upstream_tree/$cmxa
  flambda_backend_cmxa=$flambda_backend_tree/$cmxa

  ensure_exists $upstream_cmxa
  ensure_exists $flambda_backend_cmxa

  upstream=$(mktemp)
  flambda_backend=$(mktemp)

  # Implementation CRCs are not checked.

  $ocamlobjinfo $upstream_cmxa \
    | remove_digests_from_objinfo_output \
    | grep -v "^CRC of implementation" \
    > $upstream

  $ocamlobjinfo $flambda_backend_cmxa \
    | remove_digests_from_objinfo_output \
    | rewrite_flambda_backend_objinfo_c_library_names \
    | grep -v "^CRC of implementation" \
    > $flambda_backend

  $difftool <(cat $upstream | header_of_objinfo_output) \
    <(cat $flambda_backend | header_of_objinfo_output) \
    || (rm -f $upstream;
        rm -f $flambda_backend;
        exit 1
       )

  $difftool <(sort_body_of_objinfo_output_native $upstream) \
    <(sort_body_of_objinfo_output_native $flambda_backend) \
    || (rm -f $upstream;
        rm -f $flambda_backend;
        exit 1
       )

  rm -f $upstream
  rm -f $flambda_backend
}

filter_objinfo_output_for_cmx () {
  remove_digests_from_objinfo_output \
  | grep -v "^File " \
  | grep -v "^CRC of implementation" \
  | sed "s:$upstream_tree:INSTALL-DIR:" \
  | sed "s:$flambda_backend_tree:INSTALL-DIR:"
}

remove_implementations_imported_from_cmx_objinfo_output () {
  file=$1

  temp=$(mktemp)

  # For some reason this causes a non-zero exit on lexer.cmx, although
  # manually running the commands seems to work fine.
  cat $file \
    | grep -B 100000 -m 1 "^Implementations imported:" \
    > $temp \
    || true

  # Prevent spurious errors from egrep
  cd $upstream_tree

  cat $file \
    | egrep -A 100000 -m 1 \
      "^(Clambda approximation:|Flambda export information:)" \
    >> $temp \
    || true

  cp -f $temp $file
  rm -f $temp
}

check_cmx_files () {
  # We must start from the upstream tree, as we want to ensure no .cmx files
  # are missing in the Flambda backend tree.
  all_upstream_cmx=$(cd $upstream_tree && find . -name "*.cmx")

  cmx_exclusions_pipe_sep=$(echo $cmx_exclusions | sed 's/ /|/g')

  for upstream_cmx in $all_upstream_cmx; do
    dir=$(dirname $upstream_cmx)

    upstream_base=$(basename $upstream_cmx)
    # No rewriting currently required.
    flambda_backend_base=$upstream_base

    # Skip ocamldoc artifacts, those for dynlink (see comment above) and
    # profiling.cmx (which we don't support).  Likewise for main, optmain and
    # opttopstart which have different names in the Flambda backend build.
    # domainstate.cmx has some trivial location differences in the dune build
    # which at present cause comparison to fail under Flambda.
    # camlinternalMenhirLib.cmx has differences under Flambda which we ignore.
    # These propagate into parser.cmx.
    if [[ ! "$upstream_base" =~ ^.*odoc.*$ ]] \
    && [[ ! "$upstream_base" =~ ^.*dynlink.*$ ]] \
    && [[ ! "$upstream_base" =~ ^profiling.cmx$ ]] \
    && [[ ! "$upstream_base" =~ ^main.cmx$ ]] \
    && [[ ! "$upstream_base" =~ ^optmain.cmx$ ]] \
    && [[ ! "$upstream_base" =~ ^opttopstart.cmx$ ]] \
    && [[ ! "$upstream_base" =~ ^domainstate.cmx$ ]] \
    && [[ ! "$upstream_base" =~ ^camlinternalMenhirLib.cmx$ ]] \
    && [[ ! "$upstream_base" =~ ^parser.cmx$ ]] \
    && [[ ! "$upstream_base" =~ ^\($cmx_exclusions_pipe_sep\)$ ]] ;
    then
      upstream_cmx=$upstream_tree/$dir/$upstream_base
      flambda_backend_cmx=$flambda_backend_tree/$dir/$flambda_backend_base

      if [ ! -f "$flambda_backend_cmx" ]; then
        echo ".cmx file $flambda_backend_cmx is missing"
        exit 1
      fi

      upstream=$(mktemp)
      flambda_backend=$(mktemp)

      $ocamlobjinfo $upstream_cmx \
        | remove_digests_from_objinfo_output \
        | filter_objinfo_output_for_cmx \
        > $upstream

      remove_implementations_imported_from_cmx_objinfo_output $upstream

      $ocamlobjinfo $flambda_backend_cmx \
        | remove_digests_from_objinfo_output \
        | filter_objinfo_output_for_cmx \
        > $flambda_backend

      remove_implementations_imported_from_cmx_objinfo_output \
        $flambda_backend

      $difftool $upstream $flambda_backend \
        || (echo ".cmx file $flambda_backend_cmx doesn't match";
            rm -f $upstream;
            rm -f $flambda_backend;
            exit 1
           )

      rm -f $upstream
      rm -f $flambda_backend
    fi
  done
}

check_cmi_files () {
  all_upstream_cmi=$(cd $upstream_tree && find . -name "*.cmi")

  for upstream_cmi in $all_upstream_cmi; do
    dir=$(dirname $upstream_cmi)

    upstream_base=$(basename $upstream_cmi)
    flambda_backend_base=$upstream_base

    if [[ ! "$upstream_base" =~ ^.*odoc.*$ ]] \
    && [[ ! "$upstream_base" =~ ^.*dynlink.*$ ]] \
    && [[ ! "$upstream_base" =~ ^profiling.cmi$ ]] \
    && [[ ! "$upstream_base" =~ ^main.cmi$ ]] \
    && [[ ! "$upstream_base" =~ ^optmain.cmi$ ]] \
    && [[ ! "$upstream_base" =~ ^opttopmain.cmi$ ]] \
    && [[ ! "$upstream_base" =~ ^opttopstart.cmi$ ]] ;
    then
      cmi=$flambda_backend_tree/$dir/$flambda_backend_base
      if [ ! -f "$cmi" ]; then
        echo ".cmi file $cmi is missing"
        exit 1
      fi
    fi
  done
}

check_cmt_files () {
  all_upstream_cmt=$(cd $upstream_tree && find . -name "*.cmt")

  for upstream_cmt in $all_upstream_cmt; do
    dir=$(dirname $upstream_cmt)

    upstream_base=$(basename $upstream_cmt)
    flambda_backend_base=$upstream_base

    if [[ ! "$upstream_base" =~ ^.*odoc.*$ ]] \
    && [[ ! "$upstream_base" =~ ^.*dynlink.*$ ]] \
    && [[ ! "$upstream_base" =~ ^profiling.cmt$ ]] \
    && [[ ! "$upstream_base" =~ ^main.cmt$ ]] \
    && [[ ! "$upstream_base" =~ ^optmain.cmt$ ]] \
    && [[ ! "$upstream_base" =~ ^opttopmain.cmt$ ]] \
    && [[ ! "$upstream_base" =~ ^opttopstart.cmt$ ]];
    then
      cmt=$flambda_backend_tree/$dir/$flambda_backend_base
      if [ ! -f "$cmt" ]; then
        echo ".cmt file $cmt is missing"
        exit 1
      fi
    fi
  done
}

check_cmti_files () {
  all_upstream_cmti=$(cd $upstream_tree && find . -name "*.cmti")

  for upstream_cmti in $all_upstream_cmti; do
    dir=$(dirname $upstream_cmti)

    upstream_base=$(basename $upstream_cmti)
    flambda_backend_base=$upstream_base

    if [[ ! "$upstream_base" =~ ^.*odoc.*$ ]] \
    && [[ ! "$upstream_base" =~ ^.*dynlink.*$ ]] \
    && [[ ! "$upstream_base" =~ ^profiling.cmti$ ]] \
    && [[ ! "$upstream_base" =~ ^main.cmti$ ]] \
    && [[ ! "$upstream_base" =~ ^optmain.cmti$ ]] \
    && [[ ! "$upstream_base" =~ ^opttopmain.cmti$ ]];
    then
      cmti=$flambda_backend_tree/$dir/$flambda_backend_base
      if [ ! -f "$cmti" ]; then
        echo ".cmti file $cmti is missing"
        exit 1
      fi
    fi
  done
}

check_dynlink_cma_and_cmxa () {
  upstream_cma=$upstream_tree/lib/ocaml/dynlink.cma
  flambda_backend_cma=$flambda_backend_tree/lib/ocaml/dynlink.cma

  upstream_cmxa=$upstream_tree/lib/ocaml/dynlink.cmxa
  flambda_backend_cmxa=$flambda_backend_tree/lib/ocaml/dynlink.cmxa

  $difftool <($ocamlobjinfo $upstream_cma | header_of_objinfo_output) \
    <($ocamlobjinfo $flambda_backend_cma | header_of_objinfo_output)

  $difftool <($ocamlobjinfo $upstream_cmxa | header_of_objinfo_output) \
    <($ocamlobjinfo $flambda_backend_cmxa | header_of_objinfo_output)
}

# 1. Check immediate subdirs of installation root match (just the names of
# the subdirs, not the contents).

echo "** Immediate subdirs of installation root"

# The Flambda backend does not build or install man pages.
upstream_subdirs=$(ls -1 $upstream_tree | grep -v '^man$')
flambda_backend_subdirs=$(ls -1 $flambda_backend_tree)

if [ "$upstream_subdirs" != "$flambda_backend_subdirs" ]; then
  echo -e "Subdirs of install tree don't match, expected:\n$upstream_subdirs"
  exit 1
fi

# 2. Check that everything provided upstream in the bin/ directory that we
# want is present.

echo "** Executables in bin/"

upstream_bin=$(ls $upstream_tree/bin \
  | grep -v '^ocamlcp$' \
  | grep -v '^ocamlcp.byte$' \
  | grep -v '^ocamlcp.opt$' \
  | grep -v '^ocamloptp$' \
  | grep -v '^ocamloptp.byte$' \
  | grep -v '^ocamloptp.opt$' \
  | grep -v '^ocamlprof$' \
  | grep -v '^ocamlprof.byte$' \
  | grep -v '^ocamlprof.opt$' \
  )

flambda_backend_bin=$(ls $flambda_backend_tree/bin)

if [ "$upstream_bin" != "$flambda_backend_bin" ]; then
  echo "Executables in bin/ don't match:"
  $difftool <(echo $upstream_bin) <(echo $flambda_backend_bin)
  exit 1
fi

# 3. Make sure executables in lib/ocaml/ are present.

echo "** Executables in lib/ocaml/"

lib_exes="expunge extract_crc"

for exe in $lib_exes; do
  if [ ! -f $upstream_tree/lib/ocaml/$exe ]; then
    echo "Executable $exe in lib/ocaml/ not found in upstream tree"
    exit 1
  fi
  if [ ! -f $flambda_backend_tree/lib/ocaml/$exe ]; then
    echo "Executable $exe in lib/ocaml/ not found in Flambda backend tree"
    exit 1
  fi
done

# 4. Check that immediate subdirs of lib/ match (just the names, not contents).

echo "** Immediate subdirs of lib/"

upstream_subdirs=$(ls -1 $upstream_tree/lib)
flambda_backend_subdirs=$(ls -1 $flambda_backend_tree/lib)

if [ "$upstream_subdirs" != "$flambda_backend_subdirs" ]; then
  echo -e "Subdirs of install tree don't match, expected:\n$upstream_subdirs"
  exit 1
fi

# 5. Check that the VERSION files match.

echo "** VERSION files"

upstream_version=$(cat $upstream_tree/lib/ocaml/VERSION)
flambda_backend_version=$(cat $flambda_backend_tree/lib/ocaml/VERSION)

if [ "$upstream_version" != "$flambda_backend_version" ]; then
  echo -e "VERSION files in lib/ocaml/ do not match:"
  $difftool <(echo $upstream_version) <(echo $flambda_backend_version)
  exit 1
fi

# 6. Check .ml and .mli files in lib/ocaml/ and various of its subdirectories
# are all present and identical.

echo "** .ml and .mli files in lib/ocaml/ and subdirs"

compare_ml_and_mli_files "lib/ocaml"
compare_ml_and_mli_files "lib/ocaml/compiler-libs"
compare_ml_and_mli_files "lib/ocaml/threads"

# 7. Check all files in lib/ocaml/caml, the runtime headers, are identical.

echo "** All files in lib/ocaml/caml/"

upstream_files=$(cd $upstream_tree/lib/ocaml/caml/ && ls)
for file in $upstream_files; do
  if [ ! -f "$flambda_backend_tree/lib/ocaml/caml/$file" ]; then
    echo "lib/ocaml/$file is missing"
    exit 1
  fi
  $difftool $upstream_tree/lib/ocaml/caml/$file \
    $flambda_backend_tree/lib/ocaml/caml/$file
done

# 8. Check .a files match (archive filenames, archive member filenames, symbols
# defined by archive members).  Some rewrites are performed as above.

echo "** Archive filenames, members and symbols"

for archive in $archives_to_compare; do
  compare_archive $archive
done

# 9. Check .so bytecode stubs files (in lib/ocaml/stublibs/) are present
# and have the same symbols.

echo "** Bytecode stubs .so files"

for stublibs in $stublibs_to_compare; do
  compare_stublibs_or_other_dot_so_or_cmxs $stublibs "stublibs"
done

# 10. Check .cmo files are all present.
# At present the only .cmo files upstream are:
#   lib/ocaml/compiler-libs/std_exit.cmo
#   lib/ocaml/compiler-libs/main.cmo
#   lib/ocaml/compiler-libs/topstart.cmo
#   lib/ocaml/compiler-libs/optmain.cmo
#   lib/ocaml/profiling.cmo
# We don't use [Profiling] and the three driver files have different
# names in the Flambda backend build.  It seems unlikely the absence of
# these .cmo files will cause a problem.  So we just check for std_exit.cmo
# and the corresponding .o file for native use.

if [ ! -f "$upstream_tree/lib/ocaml/std_exit.o" ]; then
  echo Expected lib/ocaml/std_exit.o in upstream tree
  exit 1
fi

if [ ! -f "$flambda_backend_tree/lib/ocaml/std_exit.o" ]; then
  echo Expected lib/ocaml/std_exit.o in Flambda backend tree
  exit 1
fi

if [ ! -f "$upstream_tree/lib/ocaml/std_exit.cmo" ]; then
  echo Expected lib/ocaml/std_exit.cmo in upstream tree
  exit 1
fi

if [ ! -f "$flambda_backend_tree/lib/ocaml/std_exit.cmo" ]; then
  echo Expected lib/ocaml/std_exit.cmo in Flambda backend tree
  exit 1
fi

# 11. Check .cmx files are all present; there are also some checks on
# their contents.

echo "** .cmx files"
check_cmx_files

# 12. Check .cma files contain the same modules, flags etc.

echo "** .cma files in lib/ocaml/ and subdirs"

for cma in $cma_to_compare; do
  compare_cma_files "lib/ocaml/$cma"
done

# 13. Check .cmxa files contain the same modules, flags etc.

echo "** .cmxa files in lib/ocaml/ and subdirs"

for cmxa in $cmxa_to_compare; do
  compare_cmxa_files "lib/ocaml/$cmxa"
done

# CR mshinwell: Some of the following checks will need to be restricted
# so they exclude the ocamloptcomp build artifacts, since they will
# diverge.

# 14. Check .cmi files are all present.

echo "** .cmi files"
check_cmi_files

# 15. Check .cmt files
# It would be nice to check compilation flags, but this is difficult, not
# least because dune legitimately passes extra ones.

echo "** .cmt files"
check_cmt_files

# 16. Check .cmti files are all present.

echo "** .cmti files"
check_cmti_files

# 17. Check dynlink .cma and .cmxa flags (see comment near the top of this
# script for why we don't do a full comparison at present).
check_dynlink_cma_and_cmxa

# 18. Compare various sundry .so and .cmxs files.

echo "** Sundry .so and .cmxs files"

for shared_object in $shared_objects_to_compare; do
  compare_stublibs_or_other_dot_so_or_cmxs "$shared_object" ""
done

# 19. Remaining sundry text files of various kinds.

echo "** Sundry text files"

for file in $sundry_text_files_to_compare; do
  echo $file
  # The extra "echo"s are to suppress "No newline at end of file"
  # messages for camlheader* files.
  $difftool \
    <((cat $upstream_tree/lib/ocaml/$file \
        | sed "s:$upstream_tree:INSTALL-DIR:"); echo) \
    <((cat $flambda_backend_tree/lib/ocaml/$file \
        | sed "s:$flambda_backend_tree:INSTALL-DIR:"); echo)
done

# 20. Makefile.config comparison.

echo "** Makefile.config"

$difftool \
  <(cat $upstream_tree/lib/ocaml/Makefile.config \
      | grep -v "^CONFIGURE_ARGS=" \
      | grep -v "^WITH_OCAMLTEST=" \
      | grep -v "^STDLIB_MANPAGES=" \
      | grep -v "^prefix=") \
  <(cat $flambda_backend_tree/lib/ocaml/Makefile.config \
      | grep -v "^CONFIGURE_ARGS=" \
      | grep -v "^WITH_OCAMLTEST=" \
      | grep -v "^STDLIB_MANPAGES=" \
      | grep -v "^prefix=") \

