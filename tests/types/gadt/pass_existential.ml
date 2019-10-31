external val print : string -> unit = "print"
external val io_write : string -> unit = "io.write"
external val (-) : int -> int -> int = ""
external val (==) : int -> int -> bool = ""

type natural = Z | S of natural

type nat 'n =
  | Zero : nat Z
  | Succ : nat 'k -> nat (S 'k)

type some_nat =
  | SomeNat : nat 'n -> some_nat

let with_natural i (k : forall 'n. nat 'n -> 'b) =
  let rec go n =
    if n == 0 then
      SomeNat Zero
    else
      match go (n - 1) with
      | SomeNat x -> SomeNat (Succ x)
  in match go i with
  | SomeNat n -> k n

let main = with_natural 10 @@ fun x ->
  with_natural 20 @@ fun y ->
    let rec ppr_nat (n : nat 'n) : unit =
      match n with
      | Zero -> print "Zero"
      | Succ k ->
        io_write "Succ ";
        ppr_nat k
    ppr_nat x
    ppr_nat y
