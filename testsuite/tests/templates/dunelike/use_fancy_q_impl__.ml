module No_direct_access_to_use_fancy_q_impl = struct
  module Use_fancy_q_impl = No_such_module
end

module Use_fancy_q_impl = Use_fancy_q_impl
