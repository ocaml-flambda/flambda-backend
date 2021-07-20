module type T = sig type t val x : t end

module F1 (M: T) =
  struct
    type t = M.t option
    let x : t = Some M.x
  end

module F2 (M1: T) (M2: T) =
  F1 (struct
    type t = M1.t * M2.t
    let x = M1.x, M2.x
  end)

module F3 (M1: T) (M2: T) =
  struct
    include F2 (M1) (M2)
    let y = 0
  end
