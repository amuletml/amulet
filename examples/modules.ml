module Option = begin
  type option 'a =
    | Just of 'a
    | Nothing ;;

  let map f m =
    match m with
    | Just x -> Just (f x)
    | Nothing -> Nothing
end ;;

module Option.Helpers = begin
  open Option ;;

  let fmap = map
end ;;

module Option = begin
  open Option.Helpers
end ;;

open Option.Helpers as O ;;

let main _ = O.fmap (fun x -> x + 1) Option.Nothing
