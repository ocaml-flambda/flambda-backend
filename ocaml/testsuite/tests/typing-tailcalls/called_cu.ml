let[@inline never] add a b c d e f g h i j k l m n o p =
  a + b + c + d + e + f + g + h +
  i + j + k + l + m + n + o + p

let[@inline never] foo k = add 1 k
