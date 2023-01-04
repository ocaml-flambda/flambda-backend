type t0 = Param.t * Param.t
type t = { x : t0 }

let create_t0 p = p, Param.v
let create p = { x = create_t0 p }
let print ppf { x = (p1, p2) } =
  Format.fprintf ppf "@[<hov 1>(%a,@ %a)@]" Param.print p1 Param.print p2
