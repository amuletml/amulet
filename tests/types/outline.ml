external val ( * ) : int -> int -> int = ""

type nested 'a = Nested of nested ('a * 'a) * nested ('a * 'a) | One of 'a

let rec size (x : nested 'a) =
  match x with
  | One _ -> { count = 1 }
  | Nested (a, _) -> { count = 2 * (size a).count }

(* normally disallowed but since outlines have been improved it all
 * works out! *)
