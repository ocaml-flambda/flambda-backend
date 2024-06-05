[@@@ocaml.warning "+a-9-40-41-42"]

(** The [cmi_sign] and [cmi_globals] fields from a .cmi file, seen as a single
    term of the form:

    {v let <global name 1> = <global 1> in
       ...
       let <global name n> = <global n> in
       sig
         ...
       end v}

    Note that globals without parameters are understood to be bound but aren't
    represented explicitly. *)
type t = private {
  sign : Subst.Lazy.signature;
  bound_globals : Global_module.t array;
}

val read_from_cmi : Cmi_format.cmi_infos_lazy -> t

(** To see how substitution will work on [t], suppose we have something like
      {v let X = X in
         let Y = Y in
         let M = M{X}{Y} in
         ... X ... Y ... M ... v}
    Now suppose we import this module and pass [A] as the value of [X].

    1. We substitute [A] for [X] in the bound global names:
       {v let X = A in
          let Y = Y in
          let M = M[X:A]{Y} in
          ... X ... Y ... M ... v}
    2. Now, as usual, to work with the signature, we need to add these bindings
       to the environment. However, we can't lift them in this form, as [X] and
       [M] may have different bindings in different modules. To achieve
       consistency, we alpha-rename to a canonical form:
       {v let A = A in
          let Y = Y in
          let M[X:A] = M[X:A]{Y}
          ... A ... Y ... M[X:A] ... v}

    In general, the plan of action is:

    1. Form a substitution [S] mapping each argument's name to its value
    2. Apply [S] to the RHSes of the global name bindings
    3. For each new binding [L = R'], let [L'] be the [Global_module.to_name] of
       [R'], substitute [L'] for [L] in the body, and update the binding to
       [L' = R']

    Note that the argument values themselves won't be returned in the new list
    of bound globals, since it's assumed that they are already accounted for in
    the environment. *)
val subst : t -> (Global_module.Name.t * Global_module.t) list -> t
