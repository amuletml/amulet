type foo 'a = Foo of 'a

let implicit x = Foo 1
let given ?x = x

let bug : foo _ = (given : foo int)

(* before:
 * test.ml[6:20 ..6:24]: error
 *   Could not match expected type foo '_'ay with 'a => 'a
 *
 *   Arising in the expression
 *   │
 * 6 │ let bug : foo _ = (given : foo int)
 *   │ *)
