open import "./../amulet/exception.ml"
open import "./../amulet/option.ml"
open import "./../amulet/base.ml"
open import "./traversable.ml"
open import "./foldable.ml"

private module T = import "./internal/bbtree.ml"

private type entry 'k 'v = Entry of 'k * 'v

instance eq 'k => eq (entry 'k 'v) begin
  let Entry (a, _) == Entry (a', _) = a == a'
end

instance ord 'k => ord (entry 'k 'v) begin
  let Entry (a, _) `compare` Entry (a', _) = a `compare` a'
end

instance show 'k * show 'v => show (entry 'k 'v) begin
  let show (Entry x) = show x
end

(** The type of maps. *)
type t 'k 'v = private M of T.sz_tree (entry 'k 'v)

deriving instance typeable t

instance eq 'k * eq 'v => eq (t 'k 'v) begin
  let M x == M y = x == y
end

instance show 'k * show 'v => show (t 'k 'v) begin
  let show (M tree) = show tree
    (* let entries = T.elements tree *)
    (* "from_list " ^ show entries *)
end

(** The empty map. *)
let empty = M T.empty

(** Return the map containing the single element 'x' at key 'k'. *)
let singleton k x = M (T.singleton (Entry (k, x)))

(** Return the map containing the associations from the given list. *)
let from_list (xs : list _) =
  M (foldr (fun (k, v) xs -> T.insert (Entry (k, v)) xs) T.E xs)

(** 'alter' is the most expressive way of updating a map. Given a
 * function that can deal with both the presence of abscence of a key,
 * it upgrades that function to either inserting, updating, or deleting
 * an association in a map.
 *
 *   Inserting an element: f None     = Some x
 *   Updating an element:  f (Some x) = Some y
 *   Deleting an element:  f (Some x) = None
 *)
let alter f k (M tree) =
  let rec go = function
    | T.E -> match f None with
      | Some x -> T.singleton (Entry (k, x))
      | None   -> T.empty
    | T.T (Entry (kx, x), sx, l, r) ->
      let e = Entry (kx, x)
      match compare k kx with
      | Lt -> T.balance e (go l) r
      | Gt -> T.balance e l (go r)
      | Eq ->
        match f (Some x) with
        | Some x' -> T.T (Entry (kx, x'), sx, l, r)
        | None -> T.glue l r
  M (go tree)

(** Update the key 'k' to contain the element 'x'. *)
let insert k x (M tree) = M (T.insert (Entry (k, x)) tree)

(** Delete the key 'k' from the map. *)
let delete k (M tree) =
  M (T.delete_by (fun (Entry (k', _)) -> compare k k') tree)

(** Return the element associated with the key 'k'. *)
let lookup k (M tree) =
  let rec go = function
    | T.E -> None
    | T.T (Entry (kx, x), _, l, r) ->
      match compare k kx with
      | Eq -> Some x
      | Lt -> go l
      | Gt -> go r
  go tree
let (.?[]) map key = lookup key map

(** Lookup a key on the map. If it is not present, raise an exception.
 *)
let (.[]) map key =
  match lookup key map with
  | None   -> error "Map.(.[]): no such key in map"
  | Some x -> x

(** Update, or insert, a key on the map. Returns the new map. *)
let (.[]<-) map key new = insert key new map

instance functor (t 'k) begin
  let f <$> M tree =
    let rec go = function
      | T.E -> T.E
      | T.T (Entry (kx, x), sz, l, r) ->
          T.T (Entry (kx, f x), sz, go l, go r)
    M (go tree)
end

instance traversable (t 'k) begin
  let traverse f (M tree) =
    let rec go = function
      | T.E -> pure T.E
      | T.T (Entry (kx, x), sz, l, r) ->
        let mk x' l' r' = T.T (Entry (kx, x'), sz, l', r')
        (| mk (f x) (go l) (go r) |)
    M <$> go tree
end

(** Apply a function over every (key, value) pair in the map. The
 * function may return a new value, but not a new key. *)
let map_with_key f (M tree) =
  let rec go = function
    | T.E -> T.E
    | T.T (Entry (kx, x), sz, l, r) ->
        T.T (Entry (kx, f kx x), sz, l, r)
  M (go tree)

instance foldable (t 'k) begin
  let foldr f z (M tree) = T.inorder_fold (fun (Entry (_, x)) -> f x) z tree
end

(** Fold a map, with a function that has access to both the keys and the
 * values on the map. *)
let foldr_with_key (k : 'k -> 'a -> 'r -> 'r) (z : 'r) (M tree : t 'k 'a) =
  T.inorder_fold (fun (Entry (kx, x)) -> k kx x) z tree

(* Return the list of keys in a map. *)
let keys   (M tree) = (fun (Entry (kx, _)) -> kx) <$> T.elements tree

(* Return the list of values in a map. *)
let values (M tree) = (fun (Entry (_, x))  ->  x) <$> T.elements tree

(* Return the list of association pairs in a map. *)
let assocs (M tree) = (fun (Entry p) -> p)        <$> T.elements tree

(* Map a function over the keys of the map. Since nothing is assumed
 * about the function, it must rebuild the map structure. *)
let map_keys f xs =
  from_list @@
    ((fun (kx, x) -> (f kx, x)) <$>) @@
      assocs xs

(** Map a /monotonic/ function over the keys of a map.
 *
 * Here, monotonic means "preserves ordering", i.e.:
 *
 *  f monotonic = forall x, y. x <= y -> f x <= f y
 *
 * Therefore, this function does not have to rebuild the map structure.
 *)
let map_keys_monotonic f (M tree) =
  let rec go = function
    | T.E -> T.E
    | T.T (Entry (kx, x), sz, l, r) ->
        T.T (Entry (f kx, x), sz, go l, go r)
  M (go tree)

(** Return the least map that has the all the keys of both argument maps. *)
let union (M tree) (M tree') = M (T.union tree tree')

(** Return the set containing only the keys shared by both argument
 * map.
 *
 * If both maps have a value for the same key, the implementation
 * prefers the value of the left map.  *)
let intersection (M tree) (M tree') = M (T.intersection tree tree')

(** Return the elements of the first map that are not elements of the
 * second map.
 *
 * This function is equivalent to the following, but more efficient:
 *
 *   let difference xs ys = filter (fun x -> not (member x ys)) xs
 **)
let difference (M tree) (M tree') = M (T.difference tree tree')

(** The module 'Merge' presents a general toolkit for merging maps, with
 * greater control than union/intersection/difference. *)
module Merge = struct
  type when_missing 'k 'x 'y =
    private Missing of {
      subtree : t 'k 'x -> t 'k 'y,
      key     : 'k -> 'x  -> option 'y
    }

  type with_matched 'k 'x 'y 'z =
    private Matched of 'k -> 'x -> 'y -> option 'z

  (** If a key exists in both maps, apply a function to compute the new
   * element. *)
  let zip_with_matched f = Matched (fun k x y -> Some (f k x y))

  (** If a key exists in both maps, drop it from the result. *)
  let drop_matched = Matched (fun _ _ _ -> None)

  (** Drop all the keys from one map that aren't present in the other. *)
  let drop_missing =
    Missing { subtree = fun _ -> empty, key = fun _ _ -> None }

  (** Preserve all the keys from one map that aren't present in the
   * other. *)
  let preserve_missing =
    Missing { subtree = fun x -> x, key = fun _ -> Some }

  (** Apply a function to all the entries present in one map but present
   * in the other. *)
  let map_missing f =
    Missing { subtree = map_with_key f, key = fun k x -> Some (f k x) }

  (** Given a strategy for what to do with missing keys from either map,
   * and a strategy for what to do with keys present in both maps, merge
   * two maps.
   *
   **)
  let
      merge (Missing { subtree = g1t, key = g1k })
            (Missing { subtree = g2t })
            (Matched f)
            (M tree_l)
            (M tree_r)
    = let rec go (x : T.sz_tree _) y : T.sz_tree _ =
        match x, y with
        | t1, T.E -> let M t = g1t (M t1) in t
        | T.E, t2 -> let M t = g2t (M t2) in t
        | T.T (Entry (kx, x1), _, l1, r1), t2 ->
          let (l2, ox2, r2) =
            T.split_lookup_by (fun (Entry (ky, _)) -> compare kx ky) t2
          match ox2 with
          | None -> match g1k kx x1 with
            | Some x1' -> T.link (Entry (kx, x1')) (go l1 l2) (go r1 r2)
            | None     -> T.link2 (go l1 l2) (go r1 r2)
          | Some (Entry (_, x2)) -> match f kx x1 x2 with
            | Some x1' -> T.link (Entry (kx, x1')) (go l1 l2) (go r1 r2)
            | None     -> T.link2 (go l1 l2) (go r1 r2)
      M (go tree_l tree_r)
end

(** merge is the same function as in the Merge module. *)
let merge = Merge.merge

(** Filter a map according to a predicate with access to both the keys
 * and the values. *)
let rec filter_with_key pred (M tree) =
  let rec go =
    function
    | T.E -> T.E
    | T.T (Entry (kx, x) as entry, _, l, r) ->
      if pred kx x then
        T.link entry (go l) (go r)
      else
        T.link2 (go l) (go r)
  M (go tree)

(** Filter a map according to a predicate that does not care about the
 * keys. *)
let filter p x = filter_with_key (fun _ -> p) x

(** Partition the key according to a predicate;
 *
 * The key/value pairs for which the predicate returns true end up in
 * the first element of the returned pair, and the rest go on the second
 * element.
 **)
let rec partition_with_key pred (M tree) =
  let rec go =
    function
      | T.E -> (T.E, T.E)
      | T.T (Entry (kx, x) as entry, _, l, r) ->
        let (l1, l2) = go l
        let (r1, r2) = go r
        if pred kx x then
          (T.link entry l1 r1, T.link2 l2 r2)
        else
          (T.link2 l1 r1, T.link entry r2 r2)
  let (r1, r2) = go tree
  (M r1, M r2)
