let foo x = (x,x)
[@@zero_alloc custom_error_message "This annotation is for testing custom error messages."]


let bar x =
  if x > 0
  then x-1
  else raise (Failure (string_of_int x))
[@@zero_alloc strict custom_error_message "This annotation is for testing custom error messages."]


let fast y =
  let (x,x') as a = foo y in
  if x = x'
  then a
  else (x,y)
[@@zero_alloc opt custom_error_message
                "This annotation is for testing custom error messages \n\
                 in combination with \"opt\" payload and newlines."]


let warning_and_drop_assume x = [x;x*2]
[@@zero_alloc assume custom_error_message
                "This annotation is for testing warnings on custom error messages \
                 for \"assume\" annotations."]


let syntax_error_missing_msg x = [|x;x*2|]
[@@zero_alloc custom_error_message]


let syntax_error_missing_msg_arity x = [|x;x*2|]
[@@zero_alloc arity]
