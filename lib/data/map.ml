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

type map 'k 'v = private M of T.sz_tree (entry 'k 'v)

deriving instance typeable map

instance eq 'k * eq 'v => eq (map 'k 'v) begin
  let M x == M y = x == y
end

instance show 'k * show 'v => show (map 'k 'v) begin
  let show (M tree) =
    let entries = T.elements tree
    "from_list " ^ show entries
end

let empty = M T.empty

let singleton k x = M (T.singleton (Entry (k, x)))

let from_list (xs : list _) =
  M (foldr (fun (k, v) xs -> T.insert (Entry (k, v)) xs) T.E xs)

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

let insert k x (M tree) = M (T.insert (Entry (k, x)) tree)

let delete k (M tree) =
  M (T.delete_by (fun (Entry (k', _)) -> compare k k') tree)

let lookup k (M tree) =
  let rec go = function
    | T.E -> None
    | T.T (Entry (kx, x), _, l, r) ->
      match compare k kx with
      | Eq -> Some x
      | Lt -> go l
      | Gt -> go r
  go tree

let (.[]) map key =
  match lookup key map with
  | None   -> error "Map.(.[]): no such key in map"
  | Some x -> x

let (.[]<-) map key new = alter (fun _ -> Some new) key map

instance functor (map 'k) begin
  let f <$> M tree =
    let rec go = function
      | T.E -> T.E
      | T.T (Entry (kx, x), sz, l, r) ->
          T.T (Entry (kx, f x), sz, go l, go r)
    M (go tree)
end

instance traversable (map 'k) begin
  let traverse f (M tree) =
    let rec go = function
      | T.E -> pure T.E
      | T.T (Entry (kx, x), sz, l, r) ->
        let mk x' l' r' = T.T (Entry (kx, x'), sz, l', r')
        (| mk (f x) (go l) (go r) |)
    M <$> go tree
end

let map_with_key f (M tree) =
  let rec go = function
    | T.E -> T.E
    | T.T (Entry (kx, x), sz, l, r) ->
        T.T (Entry (kx, f kx x), sz, l, r)
  M (go tree)

instance foldable (map 'k) begin
  let foldr f z (M tree) = T.inorder_fold (fun (Entry (_, x)) -> f x) z tree
end

let foldr_with_key (k : 'k -> 'a -> 'r -> 'r) (z : 'r) (M tree : map 'k 'a) =
  T.inorder_fold (fun (Entry (kx, x)) -> k kx x) z tree

let keys   (M tree) = (fun (Entry (kx, _)) -> kx) <$> T.elements tree
let values (M tree) = (fun (Entry (_, x))  ->  x) <$> T.elements tree
let assocs (M tree) = (fun (Entry p) -> p)        <$> T.elements tree

let map_keys f xs =
  from_list @@
    ((fun (kx, x) -> (f kx, x)) <$>) @@
      assocs xs

let map_keys_monotonic f (M tree) =
  let rec go = function
    | T.E -> T.E
    | T.T (Entry (kx, x), sz, l, r) ->
        T.T (Entry (f kx, x), sz, go l, go r)
  M (go tree)

let union (M tree) (M tree') = M (T.union tree tree')
let intersection (M tree) (M tree') = M (T.intersection tree tree')
let difference (M tree) (M tree') = M (T.difference tree tree')

module Merge = struct
  type when_missing 'k 'x 'y =
    private Missing of {
      subtree : map 'k 'x -> map 'k 'y,
      key     : 'k -> 'x  -> option 'y
    }

  type with_matched 'k 'x 'y 'z =
    private Matched of 'k -> 'x -> 'y -> option 'z

  let zip_with_matched f = Matched (fun k x y -> Some (f k x y))
  let drop_matched = Matched (fun _ _ _ -> None)

  let drop_missing =
    Missing { subtree = fun _ -> empty, key = fun _ _ -> None }

  let preserve_missing =
    Missing { subtree = fun x -> x, key = fun _ -> Some }

  let map_missing f =
    Missing { subtree = map_with_key f, key = fun k x -> Some (f k x) }

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

let merge = Merge.merge

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

let filter p x = filter_with_key (fun _ -> p) x

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
