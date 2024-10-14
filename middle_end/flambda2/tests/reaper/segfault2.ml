let[@inline never] test a b c d e f g h i j k l m n o p q r s t =
  Segfault1.bar false a b c d e f g h i j k l m n o p q r s t

type 'a box = BOX of 'a
let w = test 0 (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0) (BOX 0)