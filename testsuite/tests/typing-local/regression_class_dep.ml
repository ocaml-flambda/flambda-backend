class c =
  object
    method private m () () = 0
  end

class virtual cv =
  object
    method virtual private m : unit -> unit -> int
  end
