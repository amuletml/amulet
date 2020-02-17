class foo 'a
  val get_foo : 'a

module M =
  type t = T

  instance foo t
    let get_foo = T

let M.T = get_foo
