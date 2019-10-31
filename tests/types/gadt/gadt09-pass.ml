external val print : string -> unit = "print"

type repr 'a =
  | Unit : repr unit
  | Pair : repr 'a * repr 'b -> repr ('a * 'b)
  | Int  : repr int

type option 'a = Some of 'a | None

let (<$>) f = function
  | Some x -> Some (f x)
  | None -> None

let (<*>) f x = match f, x with
  | Some f, Some x -> Some (f x)
  | _, _ -> None

let rec cast : forall 'a 'b. repr 'a -> repr 'b -> 'a -> option 'b =
  fun ra rb x ->
    match ra, rb with
    | Unit, Unit -> Some x
    | Pair (rf, rs), Pair (rf', rs') -> match x with
      | (f, s) -> (,) <$> cast rf rf' f <*> cast rs rs' s
    | Int, Int -> Some x
    | _, _ -> None

let main =
  print @@ match cast (Pair (Unit, Unit)) (Pair (Int, Int)) ((), ()) with
  | Some x -> "oh no"
  | None -> "yay"
