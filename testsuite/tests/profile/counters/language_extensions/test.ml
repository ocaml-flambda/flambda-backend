(* TEST

subdirectories = "include_functor immutable_array_comprehensions comprehensions \
                  immutable_arrays module_strengthening labeled_tuples nested";
setup-ocamlc.byte-build-env;
ocamlc_byte_exit_status = "0";
flags = "-dcounters -extension include_functor -extension comprehensions \
        -extension immutable_arrays -extension immutable_arrays \
        -extension module_strengthening -extension labeled_tuples";



(* Include functor *)
{
  module = "include_functor/ml_counters.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/include_functor/ml_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  module = "include_functor/mli_counters.mli";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/include_functor/mli_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Immutable array comprehensions *)

{
  module = "immutable_array_comprehensions/ml_counters.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/immutable_array_comprehensions/ml_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Comprehensions *)

{
  module = "comprehensions/ml_counters.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/comprehensions/ml_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  module = "comprehensions/mli_counters.mli";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/comprehensions/mli_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Immutable arrays *)

{
  module = "immutable_arrays/ml_counters.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/immutable_arrays/ml_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  module = "immutable_arrays/mli_counters.mli";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/immutable_arrays/mli_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Labeled tuples *)

{
  module = "labeled_tuples/ml_counters.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/labeled_tuples/ml_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  module = "labeled_tuples/mli_counters.mli";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/labeled_tuples/mli_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Module strengthening *)

{
  module = "module_strengthening/ml_counters.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/module_strengthening/ml_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  module = "module_strengthening/mli_counters.mli";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/module_strengthening/mli_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Nested language extensions *)

{
  module = "nested/ml_counters.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/nested/ml_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  module = "nested/mli_counters.mli";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/nested/mli_counters.ocamlc.reference";
  check-ocamlc.byte-output;
}

*)
