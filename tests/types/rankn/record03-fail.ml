type foo = Foo of { bar : forall 'a. 'a -> 'a }

let foo = Foo { bar = fun x -> x }

let main (Foo r) = r.baz 1
