(* TEST *)

type t = int

type smallrecord = {  a : t; b : t; c : t }


external opaque_local : ('a[@stack]) -> ('a[@stackret]) = "%opaque"
let ignore_local : ('a[@stack]) -> unit = fun x -> let[@stack] _ = opaque_local x in ()

let makesmall n =
  ignore_local { a = n; b = n; c = n }
let smallconst = { a = 0; b = 0; c = 0 }

let dupsmall r = ignore_local { r with a = 42 }

type bigrecord = {
  v001 : t; v002 : t; v003 : t; v004 : t; v005 : t; v006 : t; v007 : t; v008 : t; v009 : t; v010 : t;
  v011 : t; v012 : t; v013 : t; v014 : t; v015 : t; v016 : t; v017 : t; v018 : t; v019 : t; v020 : t;
  v021 : t; v022 : t; v023 : t; v024 : t; v025 : t; v026 : t; v027 : t; v028 : t; v029 : t; v030 : t;
  v031 : t; v032 : t; v033 : t; v034 : t; v035 : t; v036 : t; v037 : t; v038 : t; v039 : t; v040 : t;
  v041 : t; v042 : t; v043 : t; v044 : t; v045 : t; v046 : t; v047 : t; v048 : t; v049 : t; v050 : t;
  v051 : t; v052 : t; v053 : t; v054 : t; v055 : t; v056 : t; v057 : t; v058 : t; v059 : t; v060 : t;
  v061 : t; v062 : t; v063 : t; v064 : t; v065 : t; v066 : t; v067 : t; v068 : t; v069 : t; v070 : t;
  v071 : t; v072 : t; v073 : t; v074 : t; v075 : t; v076 : t; v077 : t; v078 : t; v079 : t; v080 : t;
  v081 : t; v082 : t; v083 : t; v084 : t; v085 : t; v086 : t; v087 : t; v088 : t; v089 : t; v090 : t;
  v091 : t; v092 : t; v093 : t; v094 : t; v095 : t; v096 : t; v097 : t; v098 : t; v099 : t; v100 : t;
  v101 : t; v102 : t; v103 : t; v104 : t; v105 : t; v106 : t; v107 : t; v108 : t; v109 : t; v110 : t;
  v111 : t; v112 : t; v113 : t; v114 : t; v115 : t; v116 : t; v117 : t; v118 : t; v119 : t; v120 : t;
  v121 : t; v122 : t; v123 : t; v124 : t; v125 : t; v126 : t; v127 : t; v128 : t; v129 : t; v130 : t;
  v131 : t; v132 : t; v133 : t; v134 : t; v135 : t; v136 : t; v137 : t; v138 : t; v139 : t; v140 : t;
  v141 : t; v142 : t; v143 : t; v144 : t; v145 : t; v146 : t; v147 : t; v148 : t; v149 : t; v150 : t;
  v151 : t; v152 : t; v153 : t; v154 : t; v155 : t; v156 : t; v157 : t; v158 : t; v159 : t; v160 : t;
  v161 : t; v162 : t; v163 : t; v164 : t; v165 : t; v166 : t; v167 : t; v168 : t; v169 : t; v170 : t;
  v171 : t; v172 : t; v173 : t; v174 : t; v175 : t; v176 : t; v177 : t; v178 : t; v179 : t; v180 : t;
  v181 : t; v182 : t; v183 : t; v184 : t; v185 : t; v186 : t; v187 : t; v188 : t; v189 : t; v190 : t;
  v191 : t; v192 : t; v193 : t; v194 : t; v195 : t; v196 : t; v197 : t; v198 : t; v199 : t; v200 : t;
  v201 : t; v202 : t; v203 : t; v204 : t; v205 : t; v206 : t; v207 : t; v208 : t; v209 : t; v210 : t;
  v211 : t; v212 : t; v213 : t; v214 : t; v215 : t; v216 : t; v217 : t; v218 : t; v219 : t; v220 : t;
  v221 : t; v222 : t; v223 : t; v224 : t; v225 : t; v226 : t; v227 : t; v228 : t; v229 : t; v230 : t;
  v231 : t; v232 : t; v233 : t; v234 : t; v235 : t; v236 : t; v237 : t; v238 : t; v239 : t; v240 : t;
  v241 : t; v242 : t; v243 : t; v244 : t; v245 : t; v246 : t; v247 : t; v248 : t; v249 : t; v250 : t;
  v251 : t; v252 : t; v253 : t; v254 : t; v255 : t; v256 : t; v257 : t; v258 : t; v259 : t; v260 : t;
  v261 : t; v262 : t; v263 : t; v264 : t; v265 : t; v266 : t; v267 : t; v268 : t; v269 : t; v270 : t;
  v271 : t; v272 : t; v273 : t; v274 : t; v275 : t; v276 : t; v277 : t; v278 : t; v279 : t; v280 : t;
  v281 : t; v282 : t; v283 : t; v284 : t; v285 : t; v286 : t; v287 : t; v288 : t; v289 : t; v290 : t;
  v291 : t; v292 : t; v293 : t; v294 : t; v295 : t; v296 : t; v297 : t; v298 : t; v299 : t; v300 : t;
}

let bigconst =
  let n = 0 in
  {
    v001 = n; v002 = n; v003 = n; v004 = n; v005 = n; v006 = n; v007 = n; v008 = n; v009 = n; v010 = n;
    v011 = n; v012 = n; v013 = n; v014 = n; v015 = n; v016 = n; v017 = n; v018 = n; v019 = n; v020 = n;
    v021 = n; v022 = n; v023 = n; v024 = n; v025 = n; v026 = n; v027 = n; v028 = n; v029 = n; v030 = n;
    v031 = n; v032 = n; v033 = n; v034 = n; v035 = n; v036 = n; v037 = n; v038 = n; v039 = n; v040 = n;
    v041 = n; v042 = n; v043 = n; v044 = n; v045 = n; v046 = n; v047 = n; v048 = n; v049 = n; v050 = n;
    v051 = n; v052 = n; v053 = n; v054 = n; v055 = n; v056 = n; v057 = n; v058 = n; v059 = n; v060 = n;
    v061 = n; v062 = n; v063 = n; v064 = n; v065 = n; v066 = n; v067 = n; v068 = n; v069 = n; v070 = n;
    v071 = n; v072 = n; v073 = n; v074 = n; v075 = n; v076 = n; v077 = n; v078 = n; v079 = n; v080 = n;
    v081 = n; v082 = n; v083 = n; v084 = n; v085 = n; v086 = n; v087 = n; v088 = n; v089 = n; v090 = n;
    v091 = n; v092 = n; v093 = n; v094 = n; v095 = n; v096 = n; v097 = n; v098 = n; v099 = n; v100 = n;
    v101 = n; v102 = n; v103 = n; v104 = n; v105 = n; v106 = n; v107 = n; v108 = n; v109 = n; v110 = n;
    v111 = n; v112 = n; v113 = n; v114 = n; v115 = n; v116 = n; v117 = n; v118 = n; v119 = n; v120 = n;
    v121 = n; v122 = n; v123 = n; v124 = n; v125 = n; v126 = n; v127 = n; v128 = n; v129 = n; v130 = n;
    v131 = n; v132 = n; v133 = n; v134 = n; v135 = n; v136 = n; v137 = n; v138 = n; v139 = n; v140 = n;
    v141 = n; v142 = n; v143 = n; v144 = n; v145 = n; v146 = n; v147 = n; v148 = n; v149 = n; v150 = n;
    v151 = n; v152 = n; v153 = n; v154 = n; v155 = n; v156 = n; v157 = n; v158 = n; v159 = n; v160 = n;
    v161 = n; v162 = n; v163 = n; v164 = n; v165 = n; v166 = n; v167 = n; v168 = n; v169 = n; v170 = n;
    v171 = n; v172 = n; v173 = n; v174 = n; v175 = n; v176 = n; v177 = n; v178 = n; v179 = n; v180 = n;
    v181 = n; v182 = n; v183 = n; v184 = n; v185 = n; v186 = n; v187 = n; v188 = n; v189 = n; v190 = n;
    v191 = n; v192 = n; v193 = n; v194 = n; v195 = n; v196 = n; v197 = n; v198 = n; v199 = n; v200 = n;
    v201 = n; v202 = n; v203 = n; v204 = n; v205 = n; v206 = n; v207 = n; v208 = n; v209 = n; v210 = n;
    v211 = n; v212 = n; v213 = n; v214 = n; v215 = n; v216 = n; v217 = n; v218 = n; v219 = n; v220 = n;
    v221 = n; v222 = n; v223 = n; v224 = n; v225 = n; v226 = n; v227 = n; v228 = n; v229 = n; v230 = n;
    v231 = n; v232 = n; v233 = n; v234 = n; v235 = n; v236 = n; v237 = n; v238 = n; v239 = n; v240 = n;
    v241 = n; v242 = n; v243 = n; v244 = n; v245 = n; v246 = n; v247 = n; v248 = n; v249 = n; v250 = n;
    v251 = n; v252 = n; v253 = n; v254 = n; v255 = n; v256 = n; v257 = n; v258 = n; v259 = n; v260 = n;
    v261 = n; v262 = n; v263 = n; v264 = n; v265 = n; v266 = n; v267 = n; v268 = n; v269 = n; v270 = n;
    v271 = n; v272 = n; v273 = n; v274 = n; v275 = n; v276 = n; v277 = n; v278 = n; v279 = n; v280 = n;
    v281 = n; v282 = n; v283 = n; v284 = n; v285 = n; v286 = n; v287 = n; v288 = n; v289 = n; v290 = n;
    v291 = n; v292 = n; v293 = n; v294 = n; v295 = n; v296 = n; v297 = n; v298 = n; v299 = n; v300 = n;
  }

let makebig n =
  let[@stack] r =
    {
      v001 = n; v002 = n; v003 = n; v004 = n; v005 = n; v006 = n; v007 = n; v008 = n; v009 = n; v010 = n;
      v011 = n; v012 = n; v013 = n; v014 = n; v015 = n; v016 = n; v017 = n; v018 = n; v019 = n; v020 = n;
      v021 = n; v022 = n; v023 = n; v024 = n; v025 = n; v026 = n; v027 = n; v028 = n; v029 = n; v030 = n;
      v031 = n; v032 = n; v033 = n; v034 = n; v035 = n; v036 = n; v037 = n; v038 = n; v039 = n; v040 = n;
      v041 = n; v042 = n; v043 = n; v044 = n; v045 = n; v046 = n; v047 = n; v048 = n; v049 = n; v050 = n;
      v051 = n; v052 = n; v053 = n; v054 = n; v055 = n; v056 = n; v057 = n; v058 = n; v059 = n; v060 = n;
      v061 = n; v062 = n; v063 = n; v064 = n; v065 = n; v066 = n; v067 = n; v068 = n; v069 = n; v070 = n;
      v071 = n; v072 = n; v073 = n; v074 = n; v075 = n; v076 = n; v077 = n; v078 = n; v079 = n; v080 = n;
      v081 = n; v082 = n; v083 = n; v084 = n; v085 = n; v086 = n; v087 = n; v088 = n; v089 = n; v090 = n;
      v091 = n; v092 = n; v093 = n; v094 = n; v095 = n; v096 = n; v097 = n; v098 = n; v099 = n; v100 = n;
      v101 = n; v102 = n; v103 = n; v104 = n; v105 = n; v106 = n; v107 = n; v108 = n; v109 = n; v110 = n;
      v111 = n; v112 = n; v113 = n; v114 = n; v115 = n; v116 = n; v117 = n; v118 = n; v119 = n; v120 = n;
      v121 = n; v122 = n; v123 = n; v124 = n; v125 = n; v126 = n; v127 = n; v128 = n; v129 = n; v130 = n;
      v131 = n; v132 = n; v133 = n; v134 = n; v135 = n; v136 = n; v137 = n; v138 = n; v139 = n; v140 = n;
      v141 = n; v142 = n; v143 = n; v144 = n; v145 = n; v146 = n; v147 = n; v148 = n; v149 = n; v150 = n;
      v151 = n; v152 = n; v153 = n; v154 = n; v155 = n; v156 = n; v157 = n; v158 = n; v159 = n; v160 = n;
      v161 = n; v162 = n; v163 = n; v164 = n; v165 = n; v166 = n; v167 = n; v168 = n; v169 = n; v170 = n;
      v171 = n; v172 = n; v173 = n; v174 = n; v175 = n; v176 = n; v177 = n; v178 = n; v179 = n; v180 = n;
      v181 = n; v182 = n; v183 = n; v184 = n; v185 = n; v186 = n; v187 = n; v188 = n; v189 = n; v190 = n;
      v191 = n; v192 = n; v193 = n; v194 = n; v195 = n; v196 = n; v197 = n; v198 = n; v199 = n; v200 = n;
      v201 = n; v202 = n; v203 = n; v204 = n; v205 = n; v206 = n; v207 = n; v208 = n; v209 = n; v210 = n;
      v211 = n; v212 = n; v213 = n; v214 = n; v215 = n; v216 = n; v217 = n; v218 = n; v219 = n; v220 = n;
      v221 = n; v222 = n; v223 = n; v224 = n; v225 = n; v226 = n; v227 = n; v228 = n; v229 = n; v230 = n;
      v231 = n; v232 = n; v233 = n; v234 = n; v235 = n; v236 = n; v237 = n; v238 = n; v239 = n; v240 = n;
      v241 = n; v242 = n; v243 = n; v244 = n; v245 = n; v246 = n; v247 = n; v248 = n; v249 = n; v250 = n;
      v251 = n; v252 = n; v253 = n; v254 = n; v255 = n; v256 = n; v257 = n; v258 = n; v259 = n; v260 = n;
      v261 = n; v262 = n; v263 = n; v264 = n; v265 = n; v266 = n; v267 = n; v268 = n; v269 = n; v270 = n;
      v271 = n; v272 = n; v273 = n; v274 = n; v275 = n; v276 = n; v277 = n; v278 = n; v279 = n; v280 = n;
      v281 = n; v282 = n; v283 = n; v284 = n; v285 = n; v286 = n; v287 = n; v288 = n; v289 = n; v290 = n;
      v291 = n; v292 = n; v293 = n; v294 = n; v295 = n; v296 = n; v297 = n; v298 = n; v299 = n; v300 = n;
    } in
  ignore_local r

let dupbig r =
  let[@stack] v = { r with v001 = 42 } in
  ignore_local v

type floatrecord = { x: float; y: float; z: float }
let makefloat n = ignore_local { x=n; y=n; z=n }
let floatconst = {x=0.; y=0.; z=0.}
let dupfloat n = ignore_local {n with x=42.}

let makepolyvariant n = ignore_local (`Foo (n, n, n))

type 'a ext = ..
type 'a ext += Foo of 'a
let makeextension n = ignore_local (Foo (n, n, n))

let makeintarray (n:int) = ignore_local [| n |]
let makeaddrarray (n:int list) = ignore_local [| n |]
let makefloatarray (n:float) = ignore_local [| n |]
let makeshortarray n = ignore_local [| n |]
let makelongarray n = ignore_local
   [| n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; |]

external local_ref : 'a -> ('a ref)[@stackret] = "%makelocalmutable"
let makeref n = ignore_local (local_ref n)

let rec makemanylong n =
  if n = 0 then () else
  let[@stack] stuff =
   [| n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n;
      n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; n; |]
  in
  makemanylong (n-1);
  ignore_local stuff

external local_array: int -> 'a -> ('a array)[@stackret] = "caml_make_local_vect"

let makeverylong n =
  ignore_local (local_array 10_000 n)

(* FIXME:
    Higher-order code:
      closures, partial applications (direct/optionals/caml_), lazy
    Leaf data:
      boxed integers, strings, bytes
*)

let run name f x =
  let prebefore = Gc.allocated_bytes () in
  let before = Gc.allocated_bytes () in
  let r = Sys.opaque_identity f x in
  let after = Gc.allocated_bytes () in
  let delta =
    int_of_float ((after -. before) -. (before -. prebefore))
      / (Sys.word_size/8)
  in
  let msg =
    match delta with
    | 0 -> "OK"
    | _ when Sys.backend_type <> Sys.Native -> "OK"
    | n -> Printf.sprintf "%d words allocated" n
  in
  Printf.printf "%15s: %s\n" name msg;
  r

let () =
  run "small" makesmall 42;
  run "dupsmall" dupsmall smallconst;
  run "big" makebig 42;
  run "dupbig" dupbig bigconst;
  run "float" makefloat 42.;
  run "dupfloat" dupfloat floatconst;
  run "polyvariant" makepolyvariant 42;
  run "extension" makeextension 42;
  run "intarray" makeintarray 42;
  run "addrarray" makeaddrarray [];
  run "floatarray" makefloatarray 42.;
  run "shortarray" makeshortarray 42;
  run "longarray" makelongarray 42;
  run "floatgenarray" makeshortarray 42.;
  run "longfgarray" makelongarray 42.;
  run "ref" makeref 42;
  run "verylong" makeverylong 42;
  run "manylong" makemanylong 100;
