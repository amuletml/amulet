Repl will be launched with the prelude

  $ echo 'put_line "Hello"' | amc repl
  Listening on port 5478
  > _ = ()
  > Hello

Not using the prelude will fail

  $ echo 'print "Hello"' | amc repl --no-prelude
  Listening on port 5478
  > =stdin[1:1 ..1:5]: error
    Variable not in scope: `print`
  
    Arising from use of the expression
    │ 
  1 │ print "Hello"
    │ ^^^^^
  > 

One can use a custom prelude

  $ echo 'print "Hello"' | amc repl --prelude=tests/amc/lib/prelude.ml
  Listening on port 5478
  > =stdin:2: Hello
  > 

Prelude is found from the library path

  $ echo 'print "Hello"' | amc repl --lib=tests/amc/lib
  Listening on port 5478
  > =stdin:2: Hello
  > 
