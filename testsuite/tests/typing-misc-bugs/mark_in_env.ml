(* TEST
   expect;
*)

(* At one point, this failed due to the fact that
   [Typemod.check_well_formed_module] puts marked types into the environment,
   causing a later substitution to fail (because a type_param wasn't
   generic). This should get fixed when
   https://github.com/ocaml/ocaml/pull/12943 is incorporated. *)

module type Row = sig
  module Row_id_part : sig
    type t

    module Map : sig
      type nonrec 'a t : immutable_data with t
    end
  end
end

module type S = sig
  module Row : Row

  module Types : sig
    module Node_or_leaf0 : sig
      type t
    end
  end

  module Node_or_leaf : sig
    val contained_leaves : Types.Node_or_leaf0.t -> int Row.Row_id_part.Map.t
  end
end

module type T = sig
  module Make (Row : Row) : S with module Row := Row
end

module Bug : T = struct
  module Make (Row : Row) = struct
    module Row_id_part = Row.Row_id_part

    module Types = struct
      module rec Node_or_leaf0 : sig
        type t = Node of string Row_id_part.Map.t
      end = struct
        type t = Node of string Row_id_part.Map.t
      end
    end

    module Node_or_leaf = struct
      let rec contained_leaves = function
        | Types.Node_or_leaf0.Node _ -> (assert false : int Row_id_part.Map.t)
      ;;
    end
  end
end

(* This should just be accepted. *)
[%%expect{|
module type Row =
  sig
    module Row_id_part :
      sig
        type t
        module Map : sig type nonrec 'a t : immutable_data with t end
      end
  end
module type S =
  sig
    module Row : Row
    module Types : sig module Node_or_leaf0 : sig type t end end
    module Node_or_leaf :
      sig
        val contained_leaves :
          Types.Node_or_leaf0.t -> int Row.Row_id_part.Map.t
      end
  end
module type T =
  sig
    module Make :
      functor (Row : Row) ->
        sig
          module Types : sig module Node_or_leaf0 : sig type t end end
          module Node_or_leaf :
            sig
              val contained_leaves :
                Types.Node_or_leaf0.t -> int Row.Row_id_part.Map.t
            end
        end
  end
Lines 42-43, characters 33-78:
42 | .................................function
43 |         | Types.Node_or_leaf0.Node _ -> (assert false : int Row_id_part.Map.t)
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of int Row_id_part.Map.t is any
         because the .cmi file for Row_id_part.Map.t is missing.
       But the layout of int Row_id_part.Map.t must be a sublayout of value
         because it has to be value for the V1 safety check.
       No .cmi file found containing Row_id_part.Map.t.
       Hint: Adding "row_id_part" to your dependencies might help.
|}, Principal{|
module type Row =
  sig
    module Row_id_part :
      sig
        type t
        module Map : sig type nonrec 'a t : immutable_data with t end
      end
  end
module type S =
  sig
    module Row : Row
    module Types : sig module Node_or_leaf0 : sig type t end end
    module Node_or_leaf :
      sig
        val contained_leaves :
          Types.Node_or_leaf0.t -> int Row.Row_id_part.Map.t
      end
  end
module type T =
  sig
    module Make :
      functor (Row : Row) ->
        sig
          module Types : sig module Node_or_leaf0 : sig type t end end
          module Node_or_leaf :
            sig
              val contained_leaves :
                Types.Node_or_leaf0.t -> int Row.Row_id_part.Map.t
            end
        end
  end
module Bug : T
|}]
