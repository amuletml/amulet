type foo = Foo of { bar : forall 'a. 'a -> 'a, quux : int }

let foo = Foo { bar = fun x -> x, quux = "foo" }
