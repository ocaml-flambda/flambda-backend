(lang dune 2.8)
(wrapped_executables false)
(using experimental_building_ocaml_compiler_with_dune 0.1)

;; CR-soon gyorsh: uncomment the following line when we upgrade to dune 2.8
;; (use_standard_c_and_cxx_flags true)
(cram enable)

(package
  (name flambda_backend)
)

(package
  (name ocaml)
)
