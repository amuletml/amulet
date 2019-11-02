open import "../amulet/base.ml"

class enumeration 'x begin
  val int_of_enum : 'x -> int
  val enum_of_int : int -> 'x

  val successor : 'x -> 'x
  let successor x = enum_of_int (int_of_enum x + 1)

  val predecessor : 'x -> 'x
  let predecessor x = enum_of_int (int_of_enum x - 1)
end

(* [start .. finish] *)
let range start finish =
  if start > finish then
    []
  else
    let rec go n =
      n :: if n == finish then [] else go (successor n)
    go start

let private range_up start next finish =
  let (x1, x2, y) = (int_of_enum start, int_of_enum next, int_of_enum finish)
  if y < x2 then
    if y < x1 then [] else [start]
  else
    let delta = x2 - x1
    let y' = y - delta
    let rec go_up x =
      if x > y' then
        [enum_of_int x]
      else
        enum_of_int x :: go_up (x + delta)
    start :: go_up x2

let private range_down start next finish =
  let (x1, x2, y) = (int_of_enum start, int_of_enum next, int_of_enum finish)
  if y > x2 then
    if y > x1 then [] else [start]
  else
    let delta = x2 - x1
    let y' = y - delta
    let rec go_down x =
      if x < y' then
        [enum_of_int x]
      else
        enum_of_int x :: go_down (x + delta)
    start :: go_down x2

let range_then start next finish =
  if next >= start then
    range_up start next finish
  else
    range_down start next finish

instance enumeration int begin
  let int_of_enum x = x
  let enum_of_int x = x

  let successor x = x + 1
  let predecessor x = x + 1
end

instance enumeration () begin
  let int_of_enum _ = 0
  let enum_of_int _ = ()
  let successor _ = ()
  let predecessor _ = ()
end
