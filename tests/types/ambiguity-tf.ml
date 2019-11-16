type option 'a = None | Some of 'a

class collects 'c begin
  type elem
  val uncons : 'c -> option (elem 'c * 'c)
end

let rec foldr f z xs =
  match uncons xs with
  | None -> z
  | Some (hd, tl) -> f hd (foldr f z tl)
