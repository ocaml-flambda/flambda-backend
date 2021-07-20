external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"

let is_valid i = (0x0000 <= i && i <= 0xD7FF) || (0xE000 <= i && i <= 0x10FFFF)
