type foo = Foo of lazy foo

let rec x : foo = Foo x
