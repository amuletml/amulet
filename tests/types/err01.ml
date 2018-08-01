type queue 'a
type option 'a = Some of 'a
let enqueue : forall 'a 'b. ('a -> option ('a * 'b)) -> 'a -> queue 'b =
  fun f x -> enqueue f x

let range start stop =
  let go (c, lim) = Some (c + 1, lim + 0)
  enqueue go (start, stop)

(* Previously, this would error with
 *
 * q.ml[58:11 ..58:12]: error
 *   Could not match expected type option ('a * 'b) with option (int * int)
 *
 *   Arising in the expression
 *    │ 
 * 58 │   enqueue go (start, stop)
 *    │
 * or something like, at least. Regression test to make sure we don't break that
 * again.
 *)
