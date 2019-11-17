type 'a + 'b = L of 'a | R of 'b
type zero = |

let zero_elim : forall 'a. zero -> 'a = function ()

let f_redundant : forall 'a. zero + 'a -> 'a = function
| L x -> zero_elim x
| R x -> x

let f_exhaustive : forall 'a. zero + 'a -> 'a = fun (R x) -> x
