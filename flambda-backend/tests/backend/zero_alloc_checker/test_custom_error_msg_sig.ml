
let expected_to_fail x = (x,x)
let expected_to_pass x = x
let do_not_check_me x = (x,x)
let check_me_in_opt x = (x,x)
let mismatched_error_msg x = (x,x)
[@@zero_alloc custom_error_message "BOOM" ]

let mismatched_error_msg2 x = (x,x)
[@@zero_alloc]

let mismatched_error_msg3 x = (x,x)
[@@zero_alloc custom_error_message "BOOM" ]

let matched_error_msg x = (x,x)
[@@zero_alloc custom_error_message "BOO" ]

let do_not_loose_the_assume x = (x,x)
[@@zero_alloc assume ]
