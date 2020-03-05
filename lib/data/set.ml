open import "./../amulet/exception.ml"
open import "./../amulet/option.ml"
open import "./../amulet/base.ml"
open import "./traversable.ml"
open import "./foldable.ml"

private module T = import "./internal/bbtree.ml"

(** The type of sets. *)
type t 'a = private S of T.sz_tree 'a

instance show 'a => show (t 'a) begin
  let show (S tree) =
    let entries = T.elements tree
    "from_list " ^ show entries
end

(** The empty set. *)
let empty = S T.empty

(** Make the set containing only the given element *)
let singleton x = S (T.singleton x)

(** Construct a 'set' from the elements of a 'list' *)
let from_list (xs : list _) = S (T.from_foldable xs)

(** Insert an element into a 'set'. *)
let insert x (S tree) = S (T.insert x tree)

(** Return the set containing everything but the given element. *)
let delete x (S tree) = S (T.delete x tree)

(** Is the given element a member of the set? *)
let member x (S tree) = T.member x tree

(** Return a list of the members of this set. *)
let members (S tree) = T.elements tree

(** Map a function over the elements of a set. This does not assume that
 * the function is monotonic, so it has to rebuild the structure of the
 * tree. *)
let map_keys f xs =
  from_list @@ (f <$>) @@ members xs

(** Map a /monotonic/ function over the elements of a set.
 *
 * Here, monotonic means "preserves ordering", i.e.:
 *
 *  f monotonic = forall x, y. x <= y -> f x <= f y
 *
 * Therefore, this function does not have to rebuild the set structure.
 *)
let map_monotonic f (S tree) =
  let rec go = function
    | T.E -> T.E
    | T.T (x, sz, l, r) -> T.T (f x, sz, go l, go r)
  S (go tree)

let foldr f z (S tree) = T.inorder_fold f z tree

(** Return the least set that has all the the elements of both argument
 * sets. *)
let union (S tree) (S tree')        = S (T.union tree tree')

(** Return the set containing only the elements shared by both argument
 * sets. *)
let intersection (S tree) (S tree') = S (T.intersection tree tree')

(** Return the elements of the first set that are not elements of the
 * second set.
 *
 * This function is equivalent to the following, but more efficient:
 *
 *   let difference xs ys = filter (fun x -> not (member x ys)) xs
 *
 * *)
let difference (S tree) (S tree')   = S (T.difference tree tree')

(** Test whether A is a subset of B: That is, every element of A is also
 * an element of B.
 * *)
let rec is_subset (S a) b =
  let rec go = function
    | T.E -> true
    | T.T (x, _, l, r) ->
      if x `member` b then
        go l && lazy (go r)
      else
        false
  go a

(** Given a predicate, return both the set of elements that satisfy that
 * predicate and the elements that don't satisfy the predicate. *)
let rec partition pred (S tree) =
  let rec go = function
  | T.E -> (T.E, T.E)
  | T.T (x, _, l, r) ->
    let (l1, l2) = go l
    let (r1, r2) = go r
    if pred x then
      (T.link x l1 r1, T.link2 l2 r2)
    else
      (T.link2 l1 r1, T.link x r2 r2)
  let (r1, r2) = go tree
  (S r1, S r2)

(** Given a predicate, return only the set of elements that satisfy that
 * predicate. *)
let rec filter pred (S tree) =
  let rec go = function
  | T.E -> T.E
  | T.T (x, _, l, r) ->
    if pred x then
      T.link x (go l) (go r)
    else
      T.link2 (go l) (go r)
  S (go tree)
