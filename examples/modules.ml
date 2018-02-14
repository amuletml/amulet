module Option =
  type option 'a =
    | Just of 'a
    | Nothing ;;

  let map f m = match m with
                | Just x -> f x
                | Nothing -> Nothing
end
;;
let main _ = Option.Nothing
