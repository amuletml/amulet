type k0
type s0
type app 'x 'y
type lit 'a
(* some inert symbols *)
type x0
type y0
type z0
type x <- lit x0
type y <- lit y0
type z <- lit z0

type stop
type more

class combine 'd1 'd2 'd | 'd1 'd2 -> 'd begin end
instance combine stop stop stop begin end
instance combine stop more more begin end
instance combine more stop more begin end
instance combine more more more begin end

class eval1 'x 'y 'd | 'x -> 'y 'd begin end
instance eval1 s0       s0       stop begin end
instance eval1 k0       k0       stop begin end
instance eval1 (lit 'a) (lit 'a) stop begin end

instance eval1 'x 'x1 'd => eval1 (app k0 'x) (app k0 'x1) 'd begin end
instance eval1 'x 'x1 'd => eval1 (app (lit 'a) 'x) (app (lit 'a) 'x1) 'd begin end

instance eval1 'x 'x1 'd => eval1 (app s0 'x) (app s0 'x1) 'd begin end

instance ( eval1 'x 'x1 'd1
         * eval1 'y 'y1 'd2
         * combine 'd1 'd2 'd
         )
      => eval1 (app (app s0 'x) 'y) (app (app s0 'x1) 'y1) 'd
  begin end


instance ( eval1 'x 'x1 'd1
         * eval1 'y 'y1 'd2
         * eval1 'z 'z1 'd3
         * combine 'd1 'd2 'd4
         * combine 'd3 'd4 'd
         )
      => eval1 (app (app (app (lit 'a) 'x) 'y) 'z)
               (app (app (app (lit 'a) 'x1) 'y1) 'z1)
               'd
  begin end

instance eval1 (app (app k0 'x) 'y) 'x more begin end
instance eval1 (app (app (app k0 'x) 'y) 'z) (app 'x 'z) more begin end

instance eval1 (app (app (app s0 'f) 'g) 'x) (app (app 'f 'x) (app 'g 'x)) more begin end

instance eval1 (app (app (app 'p 'q) 'x) 'y) 'a 'd
      => eval1 (app (app (app (app 'p 'q) 'x) 'y) 'z) (app 'a 'z) 'd
  begin end

class evalaux 'x 'y 'q1 | 'x 'q1 -> 'y begin end
instance evalaux 'x 'x stop begin end
instance ( eval1 'x 'y 'q
         * evalaux 'y 'z 'q
         )
      => evalaux 'x 'z more
  begin end

class eval 'x 'y | 'x -> 'y begin end
instance evalaux 'x 'y more => eval 'x 'y begin end

let rec eval : forall 'x 'y. eval 'x 'y => 'x -> 'y =
  fun x -> eval x

external val bot : 'a = "nil"

type kk0 <- app k0 k0
type ks0 <- app k0 s0

type z <- app s0 k0

type succ0 <- app s0 (app (app s0 ks0) k0)
type foldn 'n <- app (app 'n x) y

type succ <- app succ0
type one <- succ z
type two <- succ one
type three <- succ two

type plus0 <- app (app s0 ks0) (app (app s0 (app k0 (app s0 (app k0 s0))))
                                    (app s0 kk0))
type plus 'm 'n <- app (app plus0 'm) 'n

let x0 = eval (bot : foldn (plus two two))
