(* A rather convoluted test case, combining visible instantiation with
 * componentwise tuple subsumption *and* GADTs
 *
 * Now rejected due to impredicativity concerns! *)


external val io_write : string -> unit = "io.write"
external val to_string : 'a -> string = "tostring"

type nat = Z | S of nat
type natural 'n =
  | SZ : natural Z
  | SS : natural 'k -> natural (S 'k)

type flip 'f 'a 'b = Flip of 'f 'b 'a
let getFlip (Flip f) = f

let f @ g = fun x -> f (g x)

let ind : forall 'n. forall ('prop : nat -> type) -> 'prop Z -> (forall 'n. 'prop 'n -> 'prop (S 'n)) -> natural 'n -> 'prop 'n =
  fun z s -> function
    | SZ -> z
    | SS k -> s (ind ? z s k)

type vect 'n 'a =
  | Nil : vect Z 'a
  | Cons : 'a * vect 'k 'a -> vect (S 'k) 'a

let replicate : forall 'n 'a. 'a -> natural 'n -> vect 'n 'a = fun (x : 'a) ->
  getFlip @ ind @{flip vect 'a} (Flip Nil) (fun (Flip t) -> Flip (Cons (x, t)))

let print : forall 'a 'n. ('a -> string) -> vect 'n 'a -> unit =
  fun (f : 'a -> string) ->
    let go : forall 'n. string -> vect 'n 'a -> unit =
      fun parens -> function
        | Nil -> io_write ("Nil" ^ parens ^ "\\n")
        | Cons (x, xs) ->
            io_write ("Cons (" ^ f x ^ ", ")
            go (parens ^ ")") xs
    go ""

let main = print (fun x -> x) @@ replicate "foo" (SS (SS (SS SZ)))
