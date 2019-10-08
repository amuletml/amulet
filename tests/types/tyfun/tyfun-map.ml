type nil
type cons 'a 'b

type proxy 'a = Proxy

type slist 't =
  | SNil : slist nil
  | SCons : proxy 'a * slist 'xs -> slist (cons 'a 'xs)

type 'a ~~ 'b = Refl : 'a ~ 'b => 'a ~~ 'b

class tyfun 'f begin
  type apply : type -> type
end

type function map 'f 'xs begin
  map 'f nil = nil
  map 'f (cons 'x 'xs) = cons (apply 'f 'x) (map 'f 'xs)
end

type compose 'f 'g

instance tyfun 'f * tyfun 'g => tyfun (compose 'f 'g) begin
  type apply 'x = apply 'f (apply 'g 'x)
end

let comp_app :
    forall 'f 'g 'a. tyfun 'f * tyfun 'g
                  => proxy 'f -> proxy 'g
                  -> proxy 'a
                  -> apply (compose 'f 'g) 'a ~~ apply 'f (apply 'g 'a)
  =
    fun _ _ _ -> Refl

let foo :
    forall 'f 'g 'xs. tyfun 'f * tyfun 'g
                   => slist 'xs
                   -> map 'f (map 'g 'xs) ~~ map (compose 'f 'g) 'xs
  = function
    | SNil -> Refl
    | SCons (proxy, xs) ->
        match comp_app (Proxy : proxy 'f) (Proxy : proxy 'g) proxy with
        | Refl -> foo xs
