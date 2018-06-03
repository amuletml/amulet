let foo = lazy "oh my"
let bar = force foo
let bar' : string = foo
