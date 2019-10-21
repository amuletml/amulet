let unsafe_coerce : forall 'a 'b. 'a -> 'b =
  let foo : forall 'a 'b. ('a ~ 'b => 'a ~ 'b) => 'a -> 'b =
    fun x -> x
  fun x -> foo x

let foo : string = unsafe_coerce 10
