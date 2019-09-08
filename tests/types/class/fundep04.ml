class collects 'c 'e | 'c -> 'e begin
  val empty  : 'c
  val insert : 'c -> 'e -> 'c
end

let ins2 xs a b = insert (insert xs a) b
