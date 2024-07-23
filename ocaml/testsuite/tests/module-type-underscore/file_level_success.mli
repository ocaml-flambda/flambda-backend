module type S = module type of List

type 'a t = [] | ( :: ) of 'a * 'a t

module type T = sig
    type nonrec t = int t
end

module M = List

module type U = sig
    type nonrec t = int M.t
end
