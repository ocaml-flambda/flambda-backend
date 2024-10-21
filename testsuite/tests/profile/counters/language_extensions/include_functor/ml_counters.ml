module type M = sig
  type 'a t

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
end

module type F_M = sig
  include M

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module F_Applicative (M : M) = struct
  let map f = M.mapi (fun _ -> f)
end

module G_Applicative (M : M) = struct
  let map_i_plus_one f = M.mapi (fun i -> f (i + 1))
end

module F_Genarative (M : M) () = struct
  let map f = M.mapi (fun _ -> f)
end

module G_Generative (M : M) () = struct
  let map_i_plus_one f = M.mapi (fun i -> f (i + 1))
end


(* Include applicative functors *)

module Example_1 = struct
  type 'a t = 'a list
  let mapi = List.mapi

  include functor F_Applicative
  include functor G_Applicative
end

module Example_2 = struct
  type 'a t = 'a list
  let mapi = List.mapi

  include functor F_Applicative
end

(* Include generative functors *)

module Example_3 = struct
  type 'a t = 'a list
  let mapi = List.mapi

  include functor F_Genarative
  include functor G_Generative
end

module Example_4 = struct
  type 'a t = 'a list
  let mapi = List.mapi

  include functor F_Genarative
end

(* Standard functor usage (should not be counted) *)

module Example_5 = struct
  module A = struct
    type 'a t = 'a list
    let mapi = List.mapi
  end

  include F_Applicative (A)
  include G_Applicative (A)
end

module Example_6 = struct
  module A = struct
    type 'a t = 'a list
    let mapi = List.mapi
  end

  include F_Genarative (A) ()
  include G_Generative (A) ()
end
