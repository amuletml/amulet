type foo = Foo | Bar ;;

let main f x =
  match (let res = f x
       in begin
         res == true;
         if res then Foo else Bar
       end)
  with
  | Foo -> 1
  | Bar -> 2
