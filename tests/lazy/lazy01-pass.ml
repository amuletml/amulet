let foo = lazy (fun () -> "oh my")
let bar = force foo
let bar' : string = foo
