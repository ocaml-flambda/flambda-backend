type vec3 = {
  x : float;
  y : float;
  z : float;
}

type aabb = {
  min : vec3;
  max : vec3
}

type pos = vec3
type dir = vec3

type ray = {origin: pos; dir: dir}

let [@inline always] min (x : float) y = if x <= y then x else y
let [@inline always] max (x : float) y = if x >= y then x else y

let aabb_hit aabb (r: ray) tmin0 tmax0 =
  let [@inline always] iter min' max' origin' dir' tmin' tmax' =
    let invD = 1.0 /. dir' in
    let t0 = (min' -. origin') *. invD in
    let t1 = (max' -. origin') *. invD in
    let tmin'' = max (if invD < 0.0 then t1 else t0) tmin' in
    let tmax'' = min (if invD < 0.0 then t0 else t1) tmax' in
    (tmin'', tmax'')
  in
  let (tmin1, tmax1) =
    iter aabb.min.x aabb.max.x r.origin.x r.dir.x tmin0 tmax0
  in
  if tmax1 <= tmin1 then false
  else
    let (tmin2, tmax2) =
      iter aabb.min.y aabb.max.y r.origin.y r.dir.y tmin1 tmax1
    in
    if tmax2 <= tmin2 then false
    else
      let (tmin3, tmax3) =
        iter aabb.min.z aabb.max.z r.origin.z r.dir.z tmin2 tmax2
      in not (tmax3 <= tmin3)

