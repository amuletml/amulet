let main =
  let (id, const) = (fun x -> x, fun x _ -> x)
  id 1
  id ()
  id id
  const 1 ()
  const "foo" ()
  const {} ()
