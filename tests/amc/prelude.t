amc compile can change the prelude

  $ amc compile tests/amc/files/use_prelude.ml
  do
    local print = print
    print("Test")
  end

Not using the prelude will fail

  $ amc compile --no-prelude tests/amc/files/use_prelude.ml
  tests/amc/files/use_prelude.ml[1:10 ..1:14]: error
    Variable not in scope: `print`
  
    Arising from use of the expression
    │ 
  1 │ let () = print "Test"
    │          ^^^^^

One can use a custom prelude

  $ amc compile --prelude=tests/amc/lib/prelude.ml tests/amc/files/use_prelude.ml
  do
    local print = error
    print("Test")
  end

Prelude is found from the library path

  $ amc compile --lib=tests/amc/lib tests/amc/files/use_prelude.ml
  do
    local print = error
    print("Test")
  end

Prelude also works within the REPL

  $ echo 'print "Hello"' | amc repl
  Listening on port 5478
  > Hello
  _ = ()
  > 
