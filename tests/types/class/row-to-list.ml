type proxy 'a = Proxy

type nil
type cons ('key : string) ('type : type) 'tail

class list_to_row 'list 'row | 'list -> 'row begin end
instance list_to_row nil {} begin end
instance
     list_to_row 'tail 'rec * Amc.row_cons 'rec 'key 'type 'new
  => list_to_row (cons 'key 'type 'tail) 'new
  begin end

let list_to_row : forall 'r 'rec. list_to_row 'r 'rec => proxy 'r -> 'rec -> 'rec =
  fun _ x -> x

let x = list_to_row (Proxy : proxy (cons "x" int (cons "y" string nil))) { x = 1, y = "" }
