type foo = Foo of { bar : forall 'a. 'a -> 'a }

let foo = Foo { bar = fun x -> x + 1 }

let main (Foo r) = r.bar 1
