module Option =
  type option 'a =
    | Just of 'a
    | Nothing

  let map f m =
    match m with
    | Just x -> Just (f x)
    | Nothing -> Nothing

module Option.Helpers =
  open Option

  let fmap = map

module Option =
  open Option.Helpers
  open Helpers

module O = Option.Helpers

let _ = O.fmap (fun x -> x + 1) Option.Nothing
