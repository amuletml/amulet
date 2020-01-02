open import "prelude.ml"

type nat =
  | Z
  | S of nat

let fold num z s =
  match num with
  | Z -> z
  | S k -> s k

let pred num = fold num None Some

let rec from_int k =
  if k == 0 then Z else S (from_int (k - 1))

let rec to_int num = fold num 0 (fun x -> to_int x + 1)

instance show nat
  let show = show % to_int

let rec add n k =
  match n with
  | Z -> k
  | S n -> S (add n k)

let () = put_line ("10 + 10 is " ^ show (add (from_int 10) (from_int 10)))
