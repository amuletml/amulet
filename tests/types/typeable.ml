type foo
type bar
deriving typeable foo
deriving typeable bar

type option 'a = Some of 'a | None

let cast
  :
    forall 'a 'b. typeable 'a * typeable 'b => 'a -> option 'b
  =
    fun x ->
      eq_type_rep
        (type_of (None @'a))
        (type_of (None @'b))
        (fun _ -> Some x)
        (fun _ -> None)
