type id 'a = Id of 'a

let unId (Id x) = x

class typeable 'a => data 'a begin
  val gfoldl : (forall 'a 'b. data 'a => 'w ('a -> 'b) -> 'a -> 'w 'b)
            -> (forall 'g. 'g -> 'w 'g)
            -> 'a -> 'w 'a

  val gmapT : (forall 'b. data 'b => unit -> 'b -> 'b) -> 'a -> 'a
  let gmapT f x =
    unId (gfoldl (fun (Id c) x -> Id (c (f () x))) Id x)
end

instance data int begin
  let gfoldl _ z x = z x
end
