class functor 'f begin
  val map : ('a -> 'b) -> 'f 'a -> 'f 'b
end

let f @@ x = f x

class functor 'f => foldable 'f begin
end

class (forall 'j 'k. functor ('f 'j 'k)) => iapplicative 'f begin
  val pure : 'a -> 'f 'i 'i 'a
  val (<*>) : 'f 'i 'j ('a -> 'b) -> 'f 'j 'k 'a -> 'f 'i 'k 'b
end

class iapplicative 'm => imonad 'm begin
  val (>>=) : ('a -> 'm 'j 'k 'b) -> 'm 'i 'j 'a -> 'm 'i 'k 'b
end

type iio 'before 'after 'a = private IIO of unit -> 'a

instance functor (iio 'before 'after) begin
  let map f (IIO k) = IIO @@ fun () ->
    f (k ())
end

instance iapplicative iio begin
  let pure x = IIO (fun () -> x)
  let (IIO f) <*> (IIO v) = IIO @@ fun () ->
    let fv = f ()
    let vv = v ()
    fv vv
end
