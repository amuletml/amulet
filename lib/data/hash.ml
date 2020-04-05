open import "../prelude.ml"

class eq 'a => hashable 'a begin
  val hash : 'a -> int
end

instance hashable int begin
  let hash x = x
end

instance hashable () begin
  let hash () = 0
end

instance hashable bool begin
  let hash x = if x then 1 else 0
end

instance hashable 'a * hashable 'b => hashable ('a * 'b) begin
  let hash (a, b) = 31 * hash a + hash b
end

instance hashable string begin
  let hash xs =
    (* Hash using the djb2 hash function *)
    let open import "../lua/string.ml"
    let rec go x = function
    | 0 -> x
    | i -> go ((x * 33 + char_code_at xs i) .&. 0xFFFFFFFF) (i - 1)
    go 5381 (length xs)
end

instance hashable 'a => hashable (list 'a) begin
  let hash xs =
    let rec go h = function
    | Nil -> h
    | Cons (x, xs) -> go ((h * 31 + hash x) .&. 0xFFFFFFFF) xs
    go 0 xs
end
