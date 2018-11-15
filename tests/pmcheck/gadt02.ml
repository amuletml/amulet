type f 'a =
  | F1 : f int
  | F2 : f bool

type g 'a =
  | G1 : g int
  | G2 : g bool

class baz 'a begin
  val baz : f 'a -> g 'a
end

instance baz int begin
  let baz _ = G1
end

instance baz bool begin
  let baz _ = G2
end

(* actually exhaustive but really hard to check *)
let bar : forall 'a. baz 'a => f 'a -> () = function
  | F1 as a ->
      match baz a with
      | G1 -> ()
  | _ -> ()
