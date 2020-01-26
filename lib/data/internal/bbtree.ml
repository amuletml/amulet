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
    | T (a, _, l, r) -> "( " ^ show l ^ " <- " ^ show a ^ " -> " ^ show r ^ " )"
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

let private single_l = function
  | a, x, T (b, _, y, z) -> bin b (bin a x y) z
  | _, _, E -> error "invalid left rotation"

let private double_l = function
  | a, x, T (c, _, T (b, _, y1, y2), z) -> bin b (bin a x y1) (bin c y2 z)
  | _ -> error "invalid left double-rotation"

let private rotate_l = function
  | (_, _, T (_, _, ly, ry)) as p when size ly < ratio * size ry -> single_l p
  | p -> double_l p

let private single_r = function
  | b, T (a, _, x, y), z -> bin a x (bin b y z)
  | _, E, _ -> error "invalid right rotation"

let private double_r = function
  | c, T (a, _, x, T (b, _, y1, y2)), z -> bin b (bin a x y1) (bin c y2 z)
  | _, _, _ -> error "invalid right double-rotation"

let private rotate_r = function
  | (_, T (_, _, ly, ry), _) as p when size ly < ratio * size ry ->
      single_r p
  | p -> double_r p

let balance x l r =
  let sl = size l
  let sr = size r
  let sx = sl + sr + 1
  if sl + sr < 2 then
    T (x, sx, l, r)
  else if sr > delta * sl then
    rotate_l (x, l, r)
  else if sl > delta * sr then
    rotate_r (x, l, r)
  else T (x, sx, l, r)

let tree v l r =
  let ln = size l
  let rn = size r
  if ln + rn < 2 then
    bin v l r
  else if rn > ratio * ln then (* right tree is too big *)
    let T (_, _, rl, rr) = r
    if size rl < size rr then
      single_l (v, l, r)
    else
      double_l (v, l, r)
  else if ln > ratio * rn then
    let T (_, _, ll, lr) = l
    if size lr < size ll then
      single_r (v, l, r)
    else
      double_r (v, l, r)
  else bin v l r

let rec insert_min x = function
  | E -> singleton x
  | T (y, _, l, r) -> balance y (insert_min x l) r

let rec insert_max x = function
  | E -> singleton x
  | T (y, _, l, r) -> balance y l (insert_min x r)

let rec link x l r =
  match l, r with
  | E, r -> insert_min x r
  | l, E -> insert_max x l
  | T (y, sl, ly, ry), T (z, sr, lz, rz) ->
    if delta * sl < sr then
      balance z (link x l lz) rz
    else if delta * sr < sl then
      balance y ly (link x ry r)
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
      balance x l' r
    else
      let (x, r') = min_view rx (rl, rr)
      balance x l r'

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
    | Lt -> tree y (insert x l) r
    | Gt -> tree y l (insert x r)
    | _ -> it

let rec private cat3 = function
  | v, E, r -> insert v r
  | v, l, E -> insert v l
  | v, T (v1, n1, l1, r1) as l, T (v2, n2, l2, r2) as r ->
    if ratio * n1 < n2 then
      tree v2 (cat3 (v, l, l2)) r2
    else if ratio * n2 < n1 then
      tree v1 l1 (cat3 (v, r1, r))
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
