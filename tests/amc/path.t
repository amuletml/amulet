amc compile will extend the compile path

  $ amc compile tests/amc/files/import.ml
  tests/amc/files/import.ml[1:6 ..1:23]: error (E1010)
    Cannot resolve "my_lib.ml"
  
    Arising from use of the module
    │ 
  1 │ open import "my_lib.ml"
    │      ^^^^^^^^^^^^^^^^^^

  $ amc compile --lib tests/amc/lib tests/amc/files/import.ml
  do
  
  end
