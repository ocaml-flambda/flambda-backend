let foo x = x, x
  [@@zero_alloc
    custom_error_message "This annotation is for testing custom error messages."]

let bar x = if x > 0 then x - 1 else raise (Failure (string_of_int x))
  [@@zero_alloc
    strict custom_error_message
      "This annotation is for testing custom error messages."]

let fast y =
  let ((x, x') as a) = foo y in
  if x = x' then a else x, y
  [@@zero_alloc
    opt custom_error_message
      "This annotation is for testing custom error messages \n\
       in combination with \"opt\" payload and newlines."]

let warning_and_drop_assume x = [x; x * 2]
  [@@zero_alloc
    assume custom_error_message
      "This annotation is for testing warnings on custom error messages for \
       \"assume\" annotations."]

let syntax_error_missing_msg x = [| x; x * 2 |]
  [@@zero_alloc custom_error_message]

let syntax_error_missing_msg_arity x = [| x; x * 2 |] [@@zero_alloc arity]

(* [@zero_alloc custom_error_message "string"] in signatures: module inclusion
   results in concatenated messages. *)
module T1 = struct
  module type S1 = sig
    val f : 'a -> 'a * 'a [@@zero_alloc custom_error_message "foo"]
  end

  module type S2 = sig
    val f : 'a -> 'a * 'a [@@zero_alloc custom_error_message "bar"]
  end

  module M1 = struct
    let f x = x, x
  end

  module M' : S1 = M1

  module M'' : S2 = M1
end

(* If there is a zero_alloc annotation on the structure (with or without a
   custom error message), throw away the custom message string from the
   signature. *)
module T2 = struct
  module type S1 = sig
    val f : 'a -> 'a * 'a [@@zero_alloc custom_error_message "foo"]
  end

  module type S2 = sig
    val f : 'a -> 'a * 'a [@@zero_alloc custom_error_message "bar"]
  end

  module M2 = struct
    let[@zero_alloc] f x = x, x
  end

  module M' : S1 = M2

  module M'' : S2 = M2
end

module T3 = struct
  module type S1 = sig
    val f : 'a -> 'a * 'a [@@zero_alloc custom_error_message "foo"]
  end

  module type S2 = sig
    val f : 'a -> 'a * 'a [@@zero_alloc custom_error_message "bar"]
  end

  module M3 = struct
    let[@zero_alloc custom_error_message "use this, throw the others away"] f x
        =
      x, x
  end

  module M' : S1 = M3

  module M'' : S2 = M3
end
