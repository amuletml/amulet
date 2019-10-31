module M =
  let f () = 0

module N =
  include M
  let g () = 0

let x = 0
