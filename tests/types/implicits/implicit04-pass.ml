type foo 'a = Foo of 'a

let foo_int = Foo 1

let bar : forall 'a. foo 'a => 'a -> unit = fun (Foo _) x -> ()
let foo : forall 'a. foo 'a => 'a -> unit = fun (Foo _) x -> bar x
