let safeCoerce : forall 'a 'b. 'a ~ 'b => 'a -> 'b = fun x -> x

let foo = safeCoerce 1
