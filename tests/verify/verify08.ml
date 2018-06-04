type foo = Foo of lazy foo

let x : foo = Foo x
