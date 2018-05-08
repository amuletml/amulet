module Pipes =
  let (<|) f x = f x
  let (|>) x f = f x

  let (>>) g f x = f (g x)
  let (<<) f g x = f (g x)

let pipe x =
  let open Pipes
  x + 1
  |> (+2)
  |> (/2)

external val print : 'a -> string = "hello"


let nested x =
  let helper () =
    print "Hello"
    print "World"
  match x with
  | 1 -> print "1"
  | 2 -> print "2"
  | _ -> print "other"
  helper ()

let ifs f x =
  if x == 1 then 2
  else if x == 3
  then f x
  else
    print "Panic!"
    f x
