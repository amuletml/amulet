module F = import "./../foldable.ml"
open import "./../../amulet/base.ml"
open import "./../../amulet/option.ml"
open import "./../../amulet/exception.ml"

type sz_tree 'a =
  | E
  | T of 'a * int * sz_tree 'a * sz_tree 'a

let empty = E

let size = function
  | E -> 0
  | T (_, x, _) -> x

let singleton x = T (x, 1, E, E)

let rec member x = function
  | E -> false
  | T (y, _, l, r) ->
    match x `compare` y with
    | Lt -> x `member` l
    | Gt -> x `member` r
    | Eq -> true

let inorder_fold f zero =
  let rec go z = function
    | E -> z
    | T (x, _, l, r) -> go (f x (go z r)) l
  go zero

instance show 'a => show (sz_tree 'a) begin
  let show = function
    | E -> "E"
    | T x -> "T " ^ show x
end

instance eq 'a => eq (sz_tree 'a) begin
  let a == b =
    match a, b with
    | E, E -> true
    | T (x, sz, l, r), T (y, sz', l', r') ->
      if sz == sz' then
        x == y && l == l' && r == r'
      else
        false
    | _, _ -> false
end

let bin v l r = T (v, 1 + size l + size r, l, r)

let private delta = 3
let private ratio = 2

let balance_l (x : 'a) (l : sz_tree 'a) (r : sz_tree 'a) : sz_tree 'a =
  match r with
  | E -> match l with
    | E -> T (x, 1, E, E)
    | T (_, _, E, E) -> T (x, 2, l, E)
    | T (lx, _, E, T (lrx, _)) -> T (lrx, 3, T (lx, 1, E, E), T (x, 1, E, E))
    | T (lx, _, T _ as ll, E) -> T (lx, 3, ll, T (x, 1, E, E))
    | T (lx, ls, T (_, lls, _) as ll, T (lrx, lrs, lrl, lrr) as lr) ->
      if lrs < ratio * lls then
        T (lx, 1 + ls, ll, T (x, 1 + lrs, lr, E))
      else
        T (lrx, 1 + ls, T (lx, 1 + lls + size lrl, ll, lrl),
            T (x, 1 + size lrr, lrr, E))
  | T (_, rs, _) -> match l with
    | E -> T (x, 1 + rs, E, r)
    | T (lx, ls, ll, lr) ->
      if ls > delta * rs then
        let (T (_, lls, _), T (lrx, lrs, lrl, lrr)) = (ll, lr)
        if lrs < ratio * lls then
          T (lx, 1 + ls + rs, ll, T (x, 1 + rs + lrs, lr, r))
        else
          T (lrx, 1 + ls + rs, T (lx, 1 + lls + size lrl, ll, lrl),
              T (x, 1 + rs + size lrr, lrr, r))
      else
        T (x, 1 + ls + rs, l, r)

let balance_r (x : 'a) (l : sz_tree 'a) (r : sz_tree 'a) : sz_tree 'a =
  match l with
  | E -> match r with
    | E -> T (x, 1, E, E)
    | T (_, _, E, E) -> T (x, 2, E, r)
    | T (rx, _, E, T _ as rr) -> T (rx, 3, T (x, 1, E, E), rr)
    | T (rx, _, T (rlx, _), E) -> T (rlx, 3, T (x, 1, E, E), T (rx, 1, E, E))
    | T (rx, rs, T (rlx, rls, rll, rlr) as rl, T (_, rrs, _) as rr) ->
      if rls < ratio * rrs then
        T (rx, 1 + rs, T (x, 1 + rls, E, rl), rr)
      else
        T (rlx, 1 + rs, T (x, 1 + size rll, E, rll), T (rx, 1 + rrs + size rlr, rlr, rr))
  | T (_, ls, _) -> match r with
    | E -> T (x, 1 + ls, l, E)
    | T (rx, rs, rl, rr) ->
      if rs > delta * ls then
        let (T (rlx, rls, rll, rlr), T (_, rrs, _)) = (rl, rr)
        if rls < ratio * rrs then
          T (rx, 1 + ls + rs, T (x, 1 + ls + rls, l, rl), rr)
        else
          T (rlx, 1 + ls + rs, T (x, 1 + ls + size rll, l, rll), T (rx, 1 + rrs + size rlr, rlr, rr))
      else
        T (x, 1 + rs + ls, l, r)

let balance (x : 'a) (l : sz_tree 'a) (r : sz_tree 'a) : sz_tree 'a =
  match l with
  | E -> match r with
    | E -> T (x, 1, E, E)
    | T (_,  _, E, E) -> T (x, 2, E, r)
    | T (rx, _, E, T _ as rr) -> T (rx, 3, T (x, 1, E, E), rr)
    | T (rx, _, T (rlx, _), E) -> T (rlx, 3, T (x, 1, E, E), T (rx, 1, E, E))
    | T (rx, rs, T (rlx, rls, rll, rlr) as rl, T (_, rrs, _) as rr) ->
      if rls < ratio * rls then
        T (rx, 1 + rs, T (x, 1 + rls, E, rl), rr)
      else
        T (rlx, 1 + rs, T (x, 1 + size rll, E, rll), T (rx, 1 + rrs + size rlr, rlr, rr))
  | T (lx, ls, ll, lr) -> match r with
    | E -> match ll, lr with
      | E, E -> T (x, 2, l, E)
      | E, T (lrx, _) -> T (lrx, 3, T (lx, 1, E, E), T (x, 1, E, E))
      | T _, E -> T (lx, 3, ll, T (x, 1, E, E))
      | T (_, lls, _), T (lrx, lrs, lrl, lrr) ->
        if lrs < ratio * lls then
          T (lx, 1 + ls, ll, T (x, 1 + lrs, lr, E))
        else
          T (lrx, 1 + ls, T (lx, 1 + lls + size lrl, ll, lrl), T (x, 1 + size lrr, lrr, E))
    | T (rx, rs, rl, rr) ->
      if rs > delta * ls then
        match rl, rr with
        | T (rlx, rls, rll, rlr), T (_, rrs, _) ->
          if rls < ratio * rrs then
            T (rx, 1 + ls + rs, T (x, 1 + ls + rls, l, rl), rr)
          else
            T (rlx, 1 + ls + rs, T (x, 1 + ls + size rll, l, rll), T (rx, 1 + rrs + size rlr, rlr, rr))
        | _ -> error "Impossible: balance T vs T, rs > delta * ls"
      else if ls > delta * rs then
        match ll, lr with
        | T (_, lls, _), T (lrx, lrs, lrl, lrr) ->
          if lrs < ratio * lls then
            T (lx, 1 + ls + rs, ll, T (x, 1 + rs + lrs, lr, r))
          else
            T (lrx, 1 + ls + rs, T (lx, 1 + lls + size lrl, ll, lrl), T (x, 1 + rs + size lrr, lrr, r))
        | _ -> error "Impossible: balance T vs T, ls > delta * rs"
      else
        T (x, 1 + rs + ls, l, r)

let rec insert_min x = function
  | E -> singleton x
  | T (y, _, l, r) -> balance_l y (insert_min x l) r

let rec insert_max x = function
  | E -> singleton x
  | T (y, _, l, r) -> balance_r y l (insert_max x r)

let rec link x l r =
  match l, r with
  | E, r -> insert_min x r
  | l, E -> insert_max x l
  | T (y, sl, ly, ry), T (z, sr, lz, rz) ->
    if delta * sl < sr then
      balance_l z (link x l lz) rz
    else if delta * sr < sl then
      balance_r y ly (link x ry r)
    else
      bin x l r

let rec max_view (x : 'a) = function
  | l, E -> (x, l)
  | l, T (xr, _, rl, rr) ->
    let (xm, r') = max_view xr (rl, rr)
    (xm, balance x l r')

let rec min_view x = function
  | E, r -> (x, r)
  | T (xr, _, rl, rr), r ->
    let (xm, l') = max_view xr (rl, rr)
    (xm, balance x l' r)

let rec private glue = function
  | l, E -> l
  | E, r -> r
  | T (lx, ls, ll, lr) as l, T (rx, rs, rl, rr) as r ->
    if ls > rs then
      let (x, l') = max_view lx (ll, lr)
      balance_r x l' r
    else
      let (x, r') = min_view rx (rl, rr)
      balance_l x l r'

let rec link2 l r =
  match l, r with
  | E, r -> r
  | l, E -> l
  | T (x, sl, lx, rx) as l, T (y, sr, ly, ry) as r ->
    if delta * sl < sr then
      balance y (link2 l ly) ry
    else if delta * sr < sl then
      balance x lx (link2 rx r)
    else glue (l, r)

let glue x y = glue (x, y)

let rec delete x = function
  | E -> E
  | T (y, _, l, r) ->
    match compare x y with
    | Lt -> bin y (delete x l) r
    | Gt -> bin y l (delete x r)
    | Eq -> glue l r

let rec delete_by cmp = function
  | E -> E
  | T (y, _, l, r) ->
    match cmp y with
    | Lt -> bin y (delete_by cmp l) r
    | Gt -> bin y l (delete_by cmp r)
    | Eq -> glue l r

let rec insert x = function
  | E -> singleton x
  | T (y, _, l, r) as it ->
    match compare x y with
    | Lt -> balance_l y (insert x l) r
    | Gt -> balance_r y l (insert x r)
    | _ -> it

let rec private cat3 = function
  | v, E, r -> insert v r
  | v, l, E -> insert v l
  | v, T (v1, n1, l1, r1) as l, T (v2, n2, l2, r2) as r ->
    if ratio * n1 < n2 then
      balance_r v2 (cat3 (v, l, l2)) r2
    else if ratio * n2 < n1 then
      balance_l v1 l1 (cat3 (v, r1, r))
    else
      bin v l r

let cat3 x y z = cat3 (x, y, z)

let rec split_lt x = function
  | E -> E
  | T (v, _, l, r) ->
    match compare x v with
    | Lt -> split_lt x l
    | Gt -> cat3 v l (split_lt x r)
    | Eq -> l

let rec split_gt x = function
  | E -> E
  | T (v, _, l, r) ->
    match compare v x with
    | Lt -> split_gt x r
    | Gt -> cat3 v (split_gt x l) r
    | Eq -> r

let split_member x m =
  let rec go = function
    | E -> (E, false, E)
    | T (kx, _, l, r) ->
      match compare x kx with
      | Lt ->
        let (lt, z, gt) = go l
        (lt, z, link kx gt r)
      | Gt ->
        let (lt, z, gt) = go r
        (link kx l lt, z, gt)
      | Eq -> (l, true, r)
  go m

let split_lookup_by cmp m =
  let rec go = function
    | E -> (E, None, E)
    | T (kx, _, l, r) ->
      match cmp kx with
      | Lt ->
        let (lt, z, gt) = go l
        (lt, z, link kx gt r)
      | Gt ->
        let (lt, z, gt) = go r
        (link kx l lt, z, gt)
      | Eq -> (l, Some kx, r)
  go m

let split_lookup x m = split_lookup_by (compare x) m

let rec union x y =
  match x, y with
  | E, r -> r
  | l, E -> l
  | t1, T (x, _, l, r) ->
    let (l', _, r') = split_member x t1
    cat3 x (l' `union` l) (r' `union` r)

let rec difference x y =
  match x, y with
  | E, _ -> E
  | s, E -> s
  | t1, T (x, _, l, r) ->
    let (l', _, r') = split_member x t1
    glue (difference l' l) (difference r' r)

let rec intersection x y =
  match x, y with
  | E, _ -> E
  | _, E -> E
  | t1, T (x, _, l, r) ->
    let (l', p, r') = split_member x t1
    if p then
      cat3 x (l' `intersection` l) (r' `intersection` r)
    else
      glue (l' `intersection` l) (r' `intersection` r)

let from_foldable xs = F.foldr insert E xs

let elements (xs : sz_tree 'a) = inorder_fold (::) [] xs

let map f (xs : sz_tree 'a) = from_foldable (f <$> elements xs)

let rec map_monotonic f = function
  | E -> E
  | T (x, sz, l, r) -> T (f x, sz, map_monotonic f l, map_monotonic f r)
