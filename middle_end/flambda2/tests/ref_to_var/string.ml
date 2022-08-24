type 'a ref = { mutable contents : 'a }

external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"

external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"

external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"

external incr : (int ref[@local_opt]) -> unit = "%incr"

external decr : (int ref[@local_opt]) -> unit = "%decr"

external ( && ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool = "%sequand"

external ( / ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%divint"

external ( - ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%subint"

external ( + ) : (int[@local_opt]) -> (int[@local_opt]) -> int = "%addint"

external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"

external ( <> ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%notequal"

external ( < ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessthan"

external ( > ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterthan"

external ( <= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessequal"

external ( >= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterequal"

external compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int = "%compare"

external length : string -> int = "%string_length"

external unsafe_get : string -> int -> char = "%string_unsafe_get"

let[@inline never] sub s ofs len = assert false

let split_on_char sep s =
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = sep
    then (
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i)
  done;
  sub s 0 !j :: !r
