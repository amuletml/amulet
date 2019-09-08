external val nil : 'a = "nil"

type nil
type cons 'x 'xs

class first 'list 'x | 'list -> 'x begin end
instance first nil nil begin end
instance first (cons 'x 'more) 'x begin end

class list_cat 'a 'b 'c | 'a 'b -> 'c begin end
instance list_cat nil 'x 'x begin end
instance list_cat 'as 'bs 'cs => list_cat (cons 'a 'as) 'bs (cons 'a 'cs) begin end

class cat_all 'ls 'l | 'ls -> 'l begin end
instance cat_all nil nil begin end
instance (cat_all 'rest 'acc * list_cat 'chunk 'acc 'result) => cat_all (cons 'chunk 'rest) 'result begin end

type t
type f

class not 'b0 'b | 'b0 -> 'b begin end
instance not t f begin end
instance not f t begin end

class or 'b1 'b2 'b | 'b1 'b2 -> 'b
instance or t t t
instance or t f t
instance or f t t
instance or f f f

class any_t 'l 'r | 'l -> 'r begin end
instance any_t nil f begin end
instance any_t (cons t 'more) t begin end
instance any_t 'l 'r => any_t (cons f 'l) 'r begin end

type nat = Z | S of nat

class peq 'a 'b 't | 'a 'b -> 't begin end
instance peq Z Z t begin end
instance peq (S 'a) Z f begin end
instance peq Z (S 'a) f begin end
instance peq 'a 'b 't => peq (S 'a) (S 'b) 't begin end

class plt 'a 'b 't | 'a 'b -> 't begin end
instance plt Z Z f begin end
instance plt (S 'a) Z f begin end
instance plt Z (S 'a) t begin end
instance plt 'a 'b 't => plt (S 'a) (S 'b) 't begin end

class absdiff 'a 'b 'c | 'a 'b -> 'c begin end
instance absdiff Z Z Z begin end
instance absdiff Z (S 'a) (S 'a) begin end
instance absdiff (S 'a) Z (S 'a) begin end
instance absdiff 'a 'b 'c => absdiff (S 'a) (S 'b) 'c begin end

class range 'n 'xs | 'n -> 'xs begin end
instance range Z nil begin end
instance range 'n 'xs => range (S 'n) (cons 'n 'xs) begin end

class apply 'f 'a 'r | 'f 'a -> 'r begin end

class conj1 'list begin end
instance apply (conj1 'list) 'x (cons 'x 'list) begin end

class map 'f 'xs 'ys | 'f 'xs -> 'ys begin end
instance map 'f nil nil begin end
instance (apply 'f 'x 'y * map 'f 'xs 'ys)
  => map 'f (cons 'x 'xs) (cons 'y 'ys)

class mapcat 'f 'xs 'zs | 'f 'xs -> 'zs
instance mapcat 'f nil nil
instance (map 'f (cons 'x 'xs) 'chunks * cat_all 'chunks 'ys)
  => mapcat 'f (cons 'x 'xs) 'ys

class appendif 'pred 'x 'ys 'zs | 'pred 'x 'ys -> 'zs
instance appendif t 'x 'ys (cons 'x 'ys)
instance appendif f 'x 'ys 'ys

class filter 'f 'xs 'ys | 'f 'xs -> 'ys
instance filter 'f nil nil
instance (apply 'f 'x 't * filter 'f 'xs 'ys * appendif 't 'x 'ys 'zs)
  => filter 'f (cons 'x 'xs) 'zs begin end

type queen 'x 'y
type queen1 'x

instance apply (queen1 'x) 'y (queen 'x 'y) begin end

class queens_in 'n 'x 'queens | 'n 'x -> 'queens begin end
instance (range 'n 'ys * map (queen1 'x) 'ys 'queens) => queens_in 'n 'x 'queens begin end

class threatens 'a 'b 't | 'a 'b -> 't begin end
instance ( peq 'ax 'bx 'xeq
         * peq 'ay 'by 'yeq
         * or  'xeq 'yeq 'xyeq
         * absdiff 'ax 'bx 'dx
         * absdiff 'ay 'by 'dy
         * peq 'dx 'dy 'deq
         * or 'xyeq 'deq 'res)
  => threatens (queen 'ax 'ay) (queen 'bx 'by) 'res begin end

type threatens1 'a

instance threatens 'a 'b 't => apply (threatens1 'a) 'b 't begin end

class safe 'config 'queen 't | 'config 'queen -> 't begin end

instance (map (threatens1 'queen) 'config 'm1 * any_t 'm1 't1 * not 't1 't2)
  => safe 'config 'queen 't2 begin end

type safe1 'config
instance safe 'config 'queen 't => apply (safe1 'config) 'queen 't begin end

class addqueen 'n 'x 'c 'cs | 'n 'x 'c -> 'cs
instance ( queens_in 'n 'x 'candidates
         * filter (safe1 'c) 'candidates 'filtered
         * map (conj1 'c) 'filtered 'cs )
  => addqueen 'n 'x 'c 'cs begin end

type addqueen2 'n 'x
instance addqueen 'n 'x 'c 'cs => apply (addqueen2 'n 'x) 'c 'cs

class addq_all 'n 'x 'cs 'cs1 | 'n 'x 'cs -> 'cs1 begin end
class addqueens 'n 'x 'cs 'cs1 | 'n 'x 'cs -> 'cs1 begin end
class addq_if 'p 'n 'x 'cs 'cs1 | 'p 'n 'x 'cs -> 'cs1 begin end

instance mapcat (addqueen2 'n 'x) 'cs 'cs1 => addq_all 'n 'x 'cs 'cs1 begin end

instance addq_if f 'n 'x 'cs 'cs begin end
instance addq_all 'n 'x 'cs 'cs2 * addqueens 'n (S 'x) 'cs2 'cs1
  => addq_if t 'n 'x 'cs 'cs1 begin end

instance plt 'x 'n 'pred * addq_if 'pred 'n 'x 'cs 'cs1 => addqueens 'n 'x 'cs 'cs1

type snat 'n = SZ : snat Z | SS : snat 'n -> snat (S 'n)

class solution 'n 'c | 'n -> 'c begin
  val solution : snat 'n -> 'c
end

instance ( addqueens 'n Z (cons nil nil) 'cs * first 'cs 'c ) => solution 'n 'c begin
  let solution = nil
end

let x =
  let one = SS SZ
  let two = SS one
  let three = SS two
  let four = SS three
  solution four

(* let y : () = x *)
