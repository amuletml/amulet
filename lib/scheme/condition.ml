@cg (import (chicken condition))

@cg (define (amulet-throw desc exc)
@cg   (abort (vector 'amulet-exn desc exc)))
external val prim_throw : ('exception -> string) -> 'exception -> 'a =
  "amulet-throw"

@cg (define (amulet-catch thunk on-err)
@cg   (handle-exceptions exn
@cg      (if (and (vector? exn) (eq? 'amulet-exn (vector-ref exn 0)))
@cg        (on-err (vector-ref exn 2))
@cg        (abort exn))
@cg      (thunk (void))))
external val prim_catch : (unit -> 'a) -> ('exception -> 'a) -> 'a = "amulet-catch"
