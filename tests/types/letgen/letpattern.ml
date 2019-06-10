let () =
  let (id, const) = (fun x -> x, fun x _ -> x)
  let _ = id 1
  let _ = id ()
  let _ = id id
  let _ = const 1 ()
  let _ = const "foo" ()
  const {} ()
