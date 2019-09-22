type proxy 'a = Proxy

type nil = Nil
type cons ('key : string) ('type : type) 'tail = Cons_ of 'type * 'tail

let cons : forall 'key -> forall 'type 'tail. 'type -> 'tail -> cons 'key 'type 'tail =
  fun x t -> Cons_ (x, t)

class list_to_row 'list 'row | 'list -> 'row begin
  val make_record : 'list -> 'row
end

instance list_to_row nil {} begin
  let make_record Nil = {}
end

instance
     list_to_row 'tail 'rec * Amc.row_cons 'rec 'key 'type 'new
  => list_to_row (cons 'key 'type 'tail) 'new
begin
  let make_record (Cons_ (x, t)) = Amc.extend_row @'key x (make_record t)
end

let x = make_record (cons @"foo" 1 (cons @"bar" true Nil))
