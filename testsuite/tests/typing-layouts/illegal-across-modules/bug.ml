module type S = module type of struct
  include Shadow_stdlib
end with type 'a ref := 'a ref
