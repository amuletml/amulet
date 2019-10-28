Repl will be launched with the prelude

  $ echo 'print "Hello"' | amc repl
  Listening on port 5478
  > Hello
  _ = ()
  > 

Not using the prelude will fail

  $ echo 'print "Hello"' | amc repl --port 0 --no-prelude
  > =stdin[1:1 ..1:5]: error
    Variable not in scope: `print`
  
    Arising from use of the expression
    │ 
  1 │ print "Hello"
    │ ^^^^^
  > amc: thread killed

One can use a custom prelude

  $ echo 'print "Hello"' | amc repl --port 0 --prelude=tests/amc/lib/prelude.ml
  Listening on port 5478
  > =stdin:2: Hello
  > 

Prelude is found from the library path

  $ echo 'print "Hello"' | amc repl --port 0 --lib=tests/amc/lib
  Listening on port 5478
  > =stdin:2: Hello
  > 
