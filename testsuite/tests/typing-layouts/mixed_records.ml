(* TEST
 {
   flags = "-extension layouts_alpha";
   expect;
 }{
   flags = "-extension layouts_beta";
   expect;
 }{
   expect;
 }
*)

(* Mixed float-float# blocks are always OK. *)
type t =
  { a : float;
    b : float#;
  }

[%%expect{|
type t = { a : float; b : float#; }
|}];;

(* Mixed float-float# blocks are always OK. *)
type t =
  { a : float#;
    b : float;
  }

[%%expect{|
type t = { a : float#; b : float; }
|}];;

(* When a non-float/float# field appears, [float]
   fields are no longer considered flat. *)
type t =
  { a : float#;
    b : float;
    c : int;
  }

[%%expect{|
type t = { a : float#; b : float; c : int; }
|}];;

(* [float] appearing as a non-flat field in the value prefix. *)
type t =
  { a : float;
    b : float#;
    c : int;
  }

[%%expect{|
type t = { a : float; b : float#; c : int; }
|}];;

(* The field [c] can't be flat because a non-float/float# field [d] appears. *)
type t =
  { a : float;
    b : float#;
    c : float;
    d : int;
  }

[%%expect{|
type t = { a : float; b : float#; c : float; d : int; }
|}];;

(* Reordered mixed block: the compiler moves the string to before the flat suffix *)
type t =
  { a : float#;
    b : string;
  }

[%%expect{|
type t = { a : float#; b : string; }
|}];;

(* [f3] can be flat because all other fields are float/float#,
   so it can appear in the flat suffix.
 *)
type t =
  { f1 : float#;
    f2 : float#;
    f3 : float;
  }

[%%expect{|
type t = { f1 : float#; f2 : float#; f3 : float; }
|}];;

(* The string [f3] can't appear in the flat suffix, thus it will be moved
   by the middle end into the value prefix. *)
type t =
  { f1 : float#;
    f2 : float#;
    f3 : string;
  }

[%%expect{|
type t = { f1 : float#; f2 : float#; f3 : string; }
|}];;

(* The int [c] can appear in the flat suffix. *)
type t =
  { a : float#;
    b : float#;
    c : int;
  }

[%%expect{|
type t = { a : float#; b : float#; c : int; }
|}];;

(* Parameterized types *)

type ('a : float64) t = { x : string; y : 'a }
[%%expect{|
type ('a : float64) t = { x : string; y : 'a; }
|}];;

type ('a : float64, 'b : immediate) t = { x : string; y : 'a; z : 'b }
[%%expect{|
type ('a : float64, 'b : immediate) t = { x : string; y : 'a; z : 'b; }
|}];;

(* Types with external mode are allowed in flat suffix *)

type ('a : value mod external_) t = { x : float#; y : 'a }
type ('a : immediate) t = { x : float#; y : 'a }
[%%expect {|
type ('a : value mod external_) t = { x : float#; y : 'a; }
type ('a : immediate) t = { x : float#; y : 'a; }
|}]

type u : value mod external_
type t = { x : float#; y : u }
[%%expect {|
type u : value mod external_
type t = { x : float#; y : u; }
|}]

type u : immediate
type t = { x : float#; y : u }
[%%expect {|
type u : immediate
type t = { x : float#; y : u; }
|}]

(* Types with external mode and value layout are allowed in scannable prefix *)

type ('a : value mod external_) t = { x : 'a; y : string }
type ('a : immediate) t = { x : 'a; y : string }
[%%expect {|
type ('a : value mod external_) t = { x : 'a; y : string; }
type ('a : immediate) t = { x : 'a; y : string; }
|}]

type u : value mod external_
type t = { x : u ; y : string }
[%%expect {|
type u : value mod external_
type t = { x : u; y : string; }
|}]

type u : immediate
type t = { x : u ; y : string }
[%%expect {|
type u : immediate
type t = { x : u; y : string; }
|}]

(* Recursive groups *)

type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
[%%expect{|
type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a, 'b, 'ptr) t =
  {ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
[%%expect{|
Line 4, characters 27-37:
4 |   {ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
                               ^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is float64
           because of the definition of t_float64_id at line 1, characters 0-37.
         But the layout of 'a must overlap with value
           because it instantiates an unannotated type parameter of t,
           chosen to have layout value.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t =
  {ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
[%%expect{|
type ('a : float64) t_float = 'a t_float64_id
and ('a : immediate) t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t = {
  ptr : 'ptr;
  x : 'a;
  y : 'a t_float;
  z : 'b;
  w : 'b t_imm;
}
|}];;

(* It's illegal for the mixed representation to differ from the structure
   to the signature. This can only arise for all-float-and-float# mixed
   records, where the signature can hide the fact that a float is
   unboxed.
*)
module _ : sig
  type u
  type t = { u : u; f : float# }
  val t : t
end = struct
  type u = float
  type t = { u : float; f : float# }
  let t = { u = 3.0; f = #4.0 }
end
[%%expect {|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   type u = float
7 |   type t = { u : float; f : float# }
8 |   let t = { u = 3.0; f = #4.0 }
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig type u = float type t = { u : float; f : float#; } val t : t end
       is not included in
         sig type u type t = { u : u; f : float#; } val t : t end
       Type declarations do not match:
         type t = { u : float; f : float#; }
       is not included in
         type t = { u : u; f : float#; }
       Their internal representations differ:
       the first declaration uses a mixed representation where boxed floats are stored flat.
|}]

(* There is a cap on the number of fields in the scannable prefix. *)
type ptr = string
type t =
  {
    x1:ptr; x2:ptr; x3:ptr; x4:ptr; x5:ptr; x6:ptr; x7:ptr; x8:ptr;
    x9:ptr; x10:ptr; x11:ptr; x12:ptr; x13:ptr; x14:ptr; x15:ptr; x16:ptr;
    x17:ptr; x18:ptr; x19:ptr; x20:ptr; x21:ptr; x22:ptr; x23:ptr; x24:ptr;
    x25:ptr; x26:ptr; x27:ptr; x28:ptr; x29:ptr; x30:ptr; x31:ptr; x32:ptr;
    x33:ptr; x34:ptr; x35:ptr; x36:ptr; x37:ptr; x38:ptr; x39:ptr; x40:ptr;
    x41:ptr; x42:ptr; x43:ptr; x44:ptr; x45:ptr; x46:ptr; x47:ptr; x48:ptr;
    x49:ptr; x50:ptr; x51:ptr; x52:ptr; x53:ptr; x54:ptr; x55:ptr; x56:ptr;
    x57:ptr; x58:ptr; x59:ptr; x60:ptr; x61:ptr; x62:ptr; x63:ptr; x64:ptr;
    x65:ptr; x66:ptr; x67:ptr; x68:ptr; x69:ptr; x70:ptr; x71:ptr; x72:ptr;
    x73:ptr; x74:ptr; x75:ptr; x76:ptr; x77:ptr; x78:ptr; x79:ptr; x80:ptr;
    x81:ptr; x82:ptr; x83:ptr; x84:ptr; x85:ptr; x86:ptr; x87:ptr; x88:ptr;
    x89:ptr; x90:ptr; x91:ptr; x92:ptr; x93:ptr; x94:ptr; x95:ptr; x96:ptr;
    x97:ptr; x98:ptr; x99:ptr; x100:ptr; x101:ptr; x102:ptr; x103:ptr; x104:ptr;
    x105:ptr; x106:ptr; x107:ptr; x108:ptr; x109:ptr; x110:ptr; x111:ptr; x112:ptr;
    x113:ptr; x114:ptr; x115:ptr; x116:ptr; x117:ptr; x118:ptr; x119:ptr; x120:ptr;
    x121:ptr; x122:ptr; x123:ptr; x124:ptr; x125:ptr; x126:ptr; x127:ptr; x128:ptr;
    x129:ptr; x130:ptr; x131:ptr; x132:ptr; x133:ptr; x134:ptr; x135:ptr; x136:ptr;
    x137:ptr; x138:ptr; x139:ptr; x140:ptr; x141:ptr; x142:ptr; x143:ptr; x144:ptr;
    x145:ptr; x146:ptr; x147:ptr; x148:ptr; x149:ptr; x150:ptr; x151:ptr; x152:ptr;
    x153:ptr; x154:ptr; x155:ptr; x156:ptr; x157:ptr; x158:ptr; x159:ptr; x160:ptr;
    x161:ptr; x162:ptr; x163:ptr; x164:ptr; x165:ptr; x166:ptr; x167:ptr; x168:ptr;
    x169:ptr; x170:ptr; x171:ptr; x172:ptr; x173:ptr; x174:ptr; x175:ptr; x176:ptr;
    x177:ptr; x178:ptr; x179:ptr; x180:ptr; x181:ptr; x182:ptr; x183:ptr; x184:ptr;
    x185:ptr; x186:ptr; x187:ptr; x188:ptr; x189:ptr; x190:ptr; x191:ptr; x192:ptr;
    x193:ptr; x194:ptr; x195:ptr; x196:ptr; x197:ptr; x198:ptr; x199:ptr; x200:ptr;
    x201:ptr; x202:ptr; x203:ptr; x204:ptr; x205:ptr; x206:ptr; x207:ptr; x208:ptr;
    x209:ptr; x210:ptr; x211:ptr; x212:ptr; x213:ptr; x214:ptr; x215:ptr; x216:ptr;
    x217:ptr; x218:ptr; x219:ptr; x220:ptr; x221:ptr; x222:ptr; x223:ptr; x224:ptr;
    x225:ptr; x226:ptr; x227:ptr; x228:ptr; x229:ptr; x230:ptr; x231:ptr; x232:ptr;
    x233:ptr; x234:ptr; x235:ptr; x236:ptr; x237:ptr; x238:ptr; x239:ptr; x240:ptr;
    x241:ptr; x242:ptr; x243:ptr; x244:ptr; x245:ptr; x246:ptr; x247:ptr; x248:ptr;
    x249:ptr; x250:ptr; x251:ptr; x252:ptr; x253:ptr; x254:ptr; x255:ptr;
    unboxed:float#;
  }
[%%expect{|
type ptr = string
Lines 2-37, characters 0-3:
 2 | type t =
 3 |   {
 4 |     x1:ptr; x2:ptr; x3:ptr; x4:ptr; x5:ptr; x6:ptr; x7:ptr; x8:ptr;
 5 |     x9:ptr; x10:ptr; x11:ptr; x12:ptr; x13:ptr; x14:ptr; x15:ptr; x16:ptr;
 6 |     x17:ptr; x18:ptr; x19:ptr; x20:ptr; x21:ptr; x22:ptr; x23:ptr; x24:ptr;
...
34 |     x241:ptr; x242:ptr; x243:ptr; x244:ptr; x245:ptr; x246:ptr; x247:ptr; x248:ptr;
35 |     x249:ptr; x250:ptr; x251:ptr; x252:ptr; x253:ptr; x254:ptr; x255:ptr;
36 |     unboxed:float#;
37 |   }
Error: Mixed records may contain at most 254 value fields prior to the flat suffix, but this one contains 255.
|}];;
