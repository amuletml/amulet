let access x = x.a + 0

let access_2 x = x.a + x.b

let access_match x =
  (match x with | { a, b } -> a + b) + x.c

let access_match_2 x =
  (match x with | { a, b } -> a + b) + x.b

let access_match_3 x =
  (match x with | { a, b } -> a + b + x.c)

let () =
  let x = { a = 0, b = 0, c = 0 }

  let _ = access x
  let _ = access_2 x

  let _ = access_match x
  let _ = access_match_2 x
  let _ = access_match_3 x

  ()
