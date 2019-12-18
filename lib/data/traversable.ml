open import "../amulet/base.ml"
open import "../amulet/option.ml"
open import "../amulet/either.ml"

class functor 't => traversable 't begin
  val traverse : forall 'f. applicative 'f => ('a -> 'f 'b) -> 't 'a -> 'f ('t 'b)
  val sequence : forall 'f. applicative 'f => 't ('f 'a) -> 'f ('t 'a)

  let sequence xs = traverse (fun x -> x) xs
  let traverse cont xs = sequence (cont <$> xs)
end

instance traversable option begin
  let traverse cont = function
    | Some x -> (| Some @@ cont x |)
    | None -> (| None |)
end

instance traversable (either 'a) begin
  let traverse cont = function
    | Right a -> (| Right @@ cont a |)
    | Left b -> pure @@ Left b
end

instance traversable list begin
  let traverse cont =
    let rec loop = function
      | Cons (x, xs) -> (| cont x :: loop xs |)
      | [] -> (| [] |)
    loop
end
