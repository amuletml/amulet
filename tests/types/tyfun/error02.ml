type function foo 'a : Amc.error_message begin end

let foo : Amc.type_error (foo int) ~ int => unit -> unit = fun () -> ()
let () = foo ()
