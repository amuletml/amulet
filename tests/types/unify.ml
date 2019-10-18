type sum 'f 'g 'a =
  | L1 of 'f 'a

type k1 'c 'a = K1 of 'c
type m1 'f 'a = M1 of 'f 'a

let f x = M1 (L1 (M1 (K1 x)))

let _ : m1 (sum (m1 (k1 int)) int) () = f 123
