external val print : string -> () = "print"
external val num_to_string' : int -> string = "tostring"

let implicit num_to_string : int -> string = num_to_string'

let x |> f = f x

type list 'a =
  | Nil
  | Cons of 'a * lazy (list 'a)

let a :: b = Cons (a, b)

let zip_with f (a : list _) (b : list _) : list _ =
  match a, b with
  | Cons (x, xs), Cons (y, ys) -> f x y :: zip_with f xs ys
  | _ -> Nil

let tail (Cons (_, xs)) = xs

let take n (xs : list 'a) : list 'a =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) ->
     if n <= 0
     then Nil
     else x :: take (n - 1) xs

let list_to_string ?(to_string : 'a -> string) = function
  | Nil -> "[]"
  | Cons (x, xs) ->
     match force xs with
     | Nil -> "[" ^ to_string x ^ "]"
     | _ -> let go = function
              | Nil -> "]"
              | Cons (x, xs) -> ", " ^ to_string x ^ go (force xs)
            "[" ^ to_string x ^ go xs

let fibs = 0 :: 1 :: zip_with (+) fibs (tail fibs)

let () = take 20 fibs
         |> list_to_string
         |> print
