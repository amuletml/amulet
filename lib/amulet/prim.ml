type object

external val to_repr : 'a -> object = "function(x) return x end"
external val from_repr : object -> 'a = "function(x) return x end"

external val alloc_block : string -> object =
  "function(x) return { __tag = x } end"

external val alloc_block_with : string -> object -> object =
  "function(x, o) return { __tag = x, o } end"

external val block_get_contents : object -> object =
  "function(x) return x[1] or error('amulet/prim.ml: block_get_contents in block without contents') end"

external val block_set_contents : object -> object -> object =
  "function(x, o) x[1] = o; return x end"

external val tag_of_block : object -> string = "function(x) return x.__tag end"

let alloc_tuple x y = to_repr (x, y)

let copy_block obj =
  let tag = tag_of_block obj
  let contents = block_get_contents obj
  alloc_block_with tag contents

external val set_fst : object -> 'a -> () =
  "function(pair, x) pair._1 = x end"

external val set_snd : object -> 'b -> () =
  "function(pair, x) pair._2 = x end"
