amc compile will extend the compile path

  $ amc compile tests/amc/files/import.ml
  tests/amc/files/import.ml[1:6 ..1:23]: error (E1010)
    Cannot resolve "my_lib.ml"
    │ 
  1 │ open import "my_lib.ml"
    │      ^^^^^^^^^^^^^^^^^^
    
    Searched in:
      • .*lib/my_lib.ml(re)
      • .*lib/my_lib.ml(re)
      • .*lib/my_lib.ml(re)
  
  The following message has a detailed explanation: 1010.
  Try 'amc explain 1010' to see it.

  $ amc compile --lib tests/amc/lib tests/amc/files/import.ml
  do
  
  end
