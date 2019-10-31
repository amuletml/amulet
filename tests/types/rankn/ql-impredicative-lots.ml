external val (+) : int -> int -> int =
  "function(x, y) return x + y end"

let head (Cons (x, _)) = x
let tail (Cons (_, x)) = x
let single x = [x]

let rec append = function
  | [] -> fun ys -> ys
  | Cons (x, xs) -> fun ys -> Cons (x, append xs ys)

let rec length = function
  | [] -> 0
  | Cons (_, xs) -> 1 + length xs

type st 's 'a
external val runST : (forall 's. st 's 'a) -> 'a = "nil"
external val argST : forall 's. st 's int = "nil"

let x :: xs = Cons (x, xs)

let id x = x
let const x _ = x

let ids = (fun x -> x) :: Nil @(forall 'a. 'a -> 'a)
let inc x = x + 1
let choose (x : 'a) (_ : 'a) = x

let poly (x : forall 'a. 'a -> 'a) =
  (x 1, x true)

let map f xs = [f x | with x <- xs]
let f $ x = f x
let x |> f = f x

let a = choose Nil ids
let b = poly id
let c = poly (fun x -> x)
let d = id poly (fun x -> x)
let e = length ids
let f = tail ids
(* eta expanded because value restriction *)
let g () = head ids
let h () = (::) id
let i () = (::) ids

let j = map poly

let k : list (forall 'a. 'a -> 'a) = single id
let l = id :: ids

let m = (fun x -> x) :: ids
let n = append (single inc) (single id)
let o = map poly (single id)
let p = map head (single ids)
let q = poly $ id
let r = id |> poly

let s = runST argST
let t = runST $ argST
let u = argST |> runST

external val paper_g : forall 'a. list 'a -> list 'a -> 'a = "nil"

let v () = paper_g (single id) ids
