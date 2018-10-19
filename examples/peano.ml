external val print : 'a -> unit = "print"
external val string_of_int : int -> string = "tostring"

type nat =
  | Z
  | S of nat

type option 'a =
  | None
  | Some of 'a

let fold num z s =
  match num with
  | Z -> z
  | S k -> s k

let pred num = fold num None Some

let from_int k =
  if k == 0 then Z else S (from_int (k - 1))

let to_int num = fold num 0 (fun x -> to_int x + 1)
let print_nat num = string_of_int (to_int num)

let add n k =
  match n with
  | Z -> k
  | S n -> S (add n k)

let () = print ("10 + 10 is " ^ print_nat (add (from_int 10) (from_int 10)))
