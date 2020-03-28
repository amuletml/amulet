type option 'a = None | Some of 'a

external val ignore : 'a -> () = "ignore"

let () = ignore @@ fun { num, opt }->
  ignore @@ match num with
  | 1 | 2 | 3 -> 0
  | x -> x

  ignore @@ match opt with
  | (None, _) | (_, None) -> None
  | Some x, Some y -> Some (x, y)
