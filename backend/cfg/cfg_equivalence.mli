val check_cfg_with_layout : Mach.fundecl -> Cfg_with_layout.t -> Cfg_with_layout.t -> unit
(** [check_cfg_with_layout f expected result] checks whether [expected] and
    [result] are equivalent, failing (through [Misc.fatal_errorf]) if they
    are not.

    The passed CFGs are deemed equivalent iff:
    - the graphs are isomorphic;
    - the layouts are equal, up to the function mapping vertices in the
      isomorphism above.

    Caveat: the check currently ignores the live sets and debug information
    on all terminators, and the input registers on `Always _` terminators. *)
