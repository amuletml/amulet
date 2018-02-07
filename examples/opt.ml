(* Flattened to invalid_let = x, as inlining is incorrect *)
let invalid_let = let x = 1
                  and y = x
                  in y ;;

(* Verify complex inline statements are inlined *)
let id x = x ;;
let multi_let f = id (id (id (id (id (id (id (id (id (id (id (id (id (id (id
                  (id (id (id (id (id (id (id (id (id (id (id (id (id (id (id (f 1)))))))))))))))))))))))))))))) ;;

(* Verify changed lets are simplified *)
let multi_let' f = let x1 = f 1 in
                   let x2 = x1 in
                   x2 ;;

let main _ = ()
