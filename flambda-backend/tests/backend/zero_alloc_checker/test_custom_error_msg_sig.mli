
val expected_to_fail : 'a -> 'a*'a [@@zero_alloc custom_error_message "BOO"]
val expected_to_pass : 'a -> 'a [@@zero_alloc custom_error_message "BOO"]
val do_not_check_me  : 'a -> 'a*'a
val check_me_in_opt  : 'a -> 'a*'a [@@zero_alloc custom_error_message "BOO" opt]

val mismatched_error_msg : 'a -> 'a*'a [@@zero_alloc custom_error_message "BOO" ]
val mismatched_error_msg2 : 'a -> 'a*'a [@@zero_alloc custom_error_message "BOO" ]
val mismatched_error_msg3 : 'a -> 'a*'a [@@zero_alloc]
val matched_error_msg : 'a -> 'a*'a [@@zero_alloc custom_error_message "BOO" ]

val do_not_loose_the_assume : 'a -> 'a*'a [@@zero_alloc custom_error_message "BOO" ]
