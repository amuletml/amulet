external val print : 'a -> unit = "print"

let id x = x
let f & g = fun x -> f (g x)
let const x _ = x
let x |> f = f x

let fst (x, _) = x
let snd (_, x) = x
let uncurry f (x, y) = f x y

let fork f g x = (f x, g x)

class profunctor 'p begin
  val dimap : forall 'a 'b 'c 'd. ('b -> 'a) -> ('c -> 'd)
              -> 'p 'a 'c -> 'p 'b 'd
end

let lmap g = dimap g id
let rmap x = dimap id x

class profunctor 'p => strong 'p begin
  val first : forall 'a 'b 'c. 'p 'a 'b -> 'p ('a * 'c) ('b * 'c)
  val second : forall 'a 'b 'c. 'p 'a 'b -> 'p ('c * 'a) ('c * 'b)
end

type either 'l 'r = Left of 'l | Right of 'r

let either f g = function
  | Left x -> f x
  | Right y -> g y

class profunctor 'p => choice 'p begin
  val left : forall 'a 'b 'c. 'p 'a 'b
              -> 'p (either 'a 'c) (either 'b 'c)
  val right : forall 'a 'b 'c. 'p 'a 'b
              -> 'p (either 'c 'a) (either 'c 'b)
end

class monoid 'm begin
  val (<>) : 'm -> 'm -> 'm
  val zero : 'm
end

type forget 'r 'a 'b = Forget of 'a -> 'r
let remember (Forget r) = r

instance profunctor (->)
  let dimap f g h = g & h & f

instance strong (->)
  let first f (x, y) = (f x, y)
  let second f (x, y) = (x, f y)

instance choice (->)
  let left f = either (Left & f) Right
  let right f = either Left (Right & f)

instance profunctor (forget 'r)
  let dimap f _ (Forget g) = Forget (g & f)

instance monoid 'r => choice (forget 'r)
  let left (Forget z) = Forget (either z (const zero))
  let right (Forget z) = Forget (either (const zero) z)

instance strong (forget 'r)
  let first (Forget z) = Forget (z & fst)
  let second (Forget z) = Forget (z & snd)

let lens get set =
  dimap (fork get id) (uncurry set) & first

let view l = remember (l (Forget id))
let over f = f
let set l b = over l (const b)

type pair 'a 'b = Pair of 'a * 'b
let fst' (Pair (x, _)) = x
let snd' (Pair (_, x)) = x

let first' x = lens fst' (fun x (Pair (_, y)) -> Pair (x, y)) x
let second' x = lens snd' (fun y (Pair (x, _)) -> Pair (x, y)) x

let species x = lens (.species) (fun x r -> { r with species = x }) x
let name x = lens (.name) (fun x r -> { r with name = x }) x

let hydraz = { species = "human", name = "hydraz", age = 16 }
let _ = view name hydraz |> print
