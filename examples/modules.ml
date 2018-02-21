module Option =
  type option 'a =
    | Just of 'a
    | Nothing ;;

  let map f m = match m with
                | Just x -> Just (f x)
                | Nothing -> Nothing
end
;;

module Option.Helpers =
  open Option ;;

  let fmap = map
end
;;

open Option.Helpers as O ;;

let main _ = O.fmap (fun x -> x + 1) Option.Nothing
