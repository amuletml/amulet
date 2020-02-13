open import "../prelude.ml"

class eq 'a => hashable 'a
  val hash : 'a -> int

instance hashable int
  let hash x = x

instance hashable ()
  let hash () = 0

instance hashable bool
  let hash x = if x then 1 else 0

instance hashable 'a * hashable 'b => hashable ('a * 'b)
  let hash (a, b) = 31 * hash a + hash b

instance hashable string
  let hash xs =
    (* Hash using the djb2 hash function *)
    let open import "../lua/string.ml"
    let rec go x = function
    | 0 -> x
    | i -> go (x * 33 + char_code_at xs i) (i - 1)
    go 5381 (length xs)
