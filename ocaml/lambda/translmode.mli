open Mode
val transl_locality_mode_l : (allowed * 'r) Locality.t -> Lambda.locality_mode

val transl_alloc_mode_l : (allowed * 'r) Alloc.t -> Lambda.alloc_mode
val transl_alloc_mode_r : ('l * allowed) Alloc.t -> Lambda.alloc_mode

val transl_modify_mode : (allowed * 'r) Locality.t -> Lambda.modify_mode