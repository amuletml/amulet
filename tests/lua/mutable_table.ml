(* So technically mutation isn't supported, but this is a nice way to
   test some of the more weird functionality of Amulet *)

module Native =
  type t

  external val of_any : 'a -> t = "function(a) return a end"
  external val to_any : t -> 'a = "function(a) return a end"

module Table =
  type t 'k
  external val empty : () -> t 'k = "function() return {} end"
  external val get : t 'k -> 'k -> Native.t = "function(tbl, k) return tbl[k] end"
  external val set : t 'k -> 'k -> Native.t -> () = "function(tbl, k, v) tbl[k] = v end"

let () =
  let t = Table.empty ()
  Table.set t "foo" (Native.of_any "foo")
  Table.get t "foo"
  ()
