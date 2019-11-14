Amc will tell the user it can explain things

  $ amc compile tests/amc/files/error.ml
  tests/amc/files/error.ml[1:5 ..1:6]: error (E2001)
    │ 
  1 │ let () = 1
    │     ^^
    Couldn't match actual type int
      with the type expected by the context, unit
  
  The following message has a detailed explanation: 2001.
  Try 'amc explain 2001' to see it.

Amc will not spew 200 lines of error explanations if more than one error
is reported

  $ amc compile tests/amc/files/errors.ml
  tests/amc/files/errors.ml[4:15 ..4:16]: error (E2001)
    │ 
  4 │   let _ = 1 + ()
    │               ^^
    Couldn't match actual type unit
      with the type expected by the context, int
  tests/amc/files/errors.ml[5:42 ..5:42]: error (E2009)
    Could not match the rigid type variable 'a with the type int
  
    • Arising in this expression
    │ 
  5 │   let _ : forall 'a. 'a -> 'a = fun x -> x + 1
    │                                          ^
  
    • When checking that this expression
    │ 
  5 │   let _ : forall 'a. 'a -> 'a = fun x -> x + 1
    │                                 ^^^^^^^^^^^^^^
    has type forall 'a. 'a -> 'a
  tests/amc/files/errors.ml[5:42 ..5:46]: error (E2009)
    Could not match the rigid type variable 'a with the type int
  
    • Arising in this expression
    │ 
  5 │   let _ : forall 'a. 'a -> 'a = fun x -> x + 1
    │                                          ^^^^^
  
    • When checking that this expression
    │ 
  5 │   let _ : forall 'a. 'a -> 'a = fun x -> x + 1
    │                                 ^^^^^^^^^^^^^^
    has type forall 'a. 'a -> 'a
  
  The following messages have detailed explanations: 2001, 2009.
  Try 'amc explain 2001' to see them.
