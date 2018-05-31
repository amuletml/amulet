let foo = Lazy (fun () -> "oh my")
let bar = force foo
let bar' : string = foo
