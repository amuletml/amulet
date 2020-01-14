open import "prelude.ml"

(* The step type *)
private type step 's 'a =
  | Yield of 's * 'a
  | Skip of 's
  | Done

instance eq 's * eq 'a => eq (step 's 'a) begin
  let a == b =
    match a, b with
    | Yield (s, x), Yield (s', x') -> x == x' && s == s'
    | Skip s, Skip s' -> s == s'
    | Done, Done -> true
    | _, _ -> false
end

(** The abstract type of streams. *)
type stream 'a =
  private Stream : forall 's. ('s -> step 's 'a) * 's -> stream 'a

instance show 'a => show (stream 'a) begin
  let show (Stream (step, start)) =
    let show_e (x : 'a) = show x
    let init = 10
    let rec go lim acc st =
      match step st, lim with
      | Done, _ -> acc ^ "]"
      | _, 0    -> acc ^ ", ..."
      | Yield (s, x), n when lim == init ->
          go (n - 1) (acc ^ show_e x) s
      | Yield (s, x), n ->
          go (n - 1) (acc ^ ", " ^ show_e x) s
      | Skip s, n -> go n acc s
    go 10 "[" start
end

instance functor (step 's) begin
  let f <$> xs =
    match xs with
    | Yield (s, a) -> Yield (s, f a)
    | Skip s -> Skip s
    | Done -> Done
end

instance functor stream begin
  let f <$> Stream (step, start) =
    Stream (fun x -> f <$> step x, start)
end

instance applicative stream begin
  let pure x = Stream (fun _ -> Yield ((), x), ())
  let Stream (step_a, start_a) <*> Stream (step_b, start_b) = 
    let step = function
      | sa, sb, None ->
          match step_a sa with
          | Done -> Done
          | Yield (sa', f) -> Skip (sa', sb, Some f)
          | Skip sa' -> Skip (sa', sb, None)
      | sa, sb, Some f ->
          match step_b sb with
          | Done -> Done
          | Yield (sb', x) -> Yield ((sa, sb', None), f x)
          | Skip sb' -> Skip (sa, sb', None)
    Stream (step, start_a, start_b, None)
end

instance foldable stream begin
  let foldl f z (Stream (step, start)) =
    let rec loop acc state =
      match step state with
      | Done -> acc
      | Skip s -> loop acc s
      | Yield (s, x) -> loop (f acc x) s
    loop z start

  let foldr f z (Stream (step, start)) =
    let rec loop k state =
      match step state with
      | Done -> k z
      | Skip s -> loop k s
      | Yield (s, x) -> loop (k % f x) s
    loop id start
end

let take n (Stream (step, start)) =
  let step (count, st) =
    if count > 0 then
      match step st with
      | Yield (s, x) -> Yield ((count - 1, s), x)
      | Skip s -> Skip (count, s)
      | Done -> Done
    else
      Done
  Stream (step, (n, start))

let drop n (Stream (step, start)) =
  let step (count, st) =
    if count > 0 then
      match step st with
      | Yield (s, _) -> Skip (count - 1, s)
      | Skip s -> Skip (count, s)
      | Done -> Done
    else
      match step st with
      | Yield (s, x) -> Yield ((0, s), x)
      | Skip s -> Skip (0, s)
      | Done -> Done
  Stream (step, (n, start))

let x :> xs =
  let step = function
    | None -> Yield (Some xs, x)
    | Some xs ->
        let Stream (step, start) = force xs
        match step start with
        | Skip s -> Skip (Some (lazy (Stream (step, s))))
        | Yield (s, x) -> Yield (Some (lazy (Stream (step, s))), x)
        | Done -> Done
  Stream (step, None)

let nil = Stream (fun _ -> Done, ())

let unfoldr z f =
  let step st =
    match f st with
    | Some (vl, st) -> Yield (st, vl)
    | None -> Done
  Stream (step, z)

let range start fin =
  let delta = if fin > start then 1 else -1
  let cmp = if fin > start then (<=) else (>=)
  let step i =
    if i `cmp` fin then
      Yield (i + delta, i)
    else
      Done
  Stream (step, start)

let range_then start next fin =
  let delta = if fin > start then next - start else start - next
  let cmp = if fin > start then (<=) else (>=)
  let step i =
    if i `cmp` fin then
      Yield (i + delta, i)
    else
      Done
  Stream (step, start)

let from_list xs =
  let uncons = function
    | [] -> None
    | Cons (x, xs) -> Some (x, xs)
  unfoldr xs uncons

let filter p (Stream (step, start)) =
  let step st =
    match step st with
    | Yield (s, x) when p x -> Yield (s, x)
    | Yield (s, _) -> Skip s | Skip s -> Skip s
    | Done -> Done
  Stream (step, start)

let take_while p (Stream (step, start)) =
  let step st =
    match step st with
    | Yield (s, x) when p x -> Yield (s, x)
    | Yield _ -> Done
    | Skip s -> Skip s
    | Done -> Done
  Stream (step, start)

let drop_while p (Stream (step, start)) =
  let step = function
    | true, st ->
      match step st with
      | Yield (s, x) when p x -> Skip (true, s)
      | Yield (s, x) -> Yield ((false, s), x)
      | Skip s -> Skip (true, s)
      | Done -> Done
    | false, st ->
      match step st with
      | Yield (s, x) -> Yield ((false, s), x)
      | Skip s -> Skip (false, s)
      | Done -> Done
  Stream (step, (true, start))

let uncons (Stream (step, start)) =
  let rec go st =
    match step st with
    | Done -> None
    | Skip s -> go s
    | Yield (s, x) -> Some (x, Stream (step, s))
  go start

let indexed (Stream (step, start)) =
  let step (i, st) =
    match step st with
    | Yield (st, x) -> Yield ((i + 1, st), (i, x))
    | Skip st -> Skip (i, st)
    | Done -> Done
  Stream (step, (1, start))

let Stream (step_a, start_a) <+> Stream (step_b, start_b) =
  let step = function
    | Left s ->
        match step_a s with
        | Done -> Skip (Right start_b)
        | Yield (s, x) -> Yield (Left s, x)
        | Skip s -> Skip (Left s)
    | Right s ->
        match step_b s with
        | Done -> Done
        | Yield (s, x) -> Yield (Right s, x)
        | Skip s -> Skip (Right s)
  Stream (step, Left start_a)

let any_in_seq (p : 'a -> _) (Stream (step, start)) =
  let rec go st =
    match step st with
    | Done -> false
    | Skip s -> go s
    | Yield (s, x) ->
        if p x then
          true
        else
          go s
  go start

let all_in_seq (p : 'a -> _) (Stream (step, start)) =
  let rec go st =
    match step st with
    | Done -> true
    | Skip s -> go s
    | Yield (s, x) ->
        if p x then
          go s
        else
          false
  go start

let iterate f x = Stream (fun x -> Yield (x, f x), x)

let replicate n x =
  let next i =
    if i <= 0 then
      Done
    else
      Yield (i - 1, x)
  Stream (next, n)

let lookup k (Stream (step, start)) =
  let rec loop k s =
    match step s with
    | Yield (s, (k', x)) ->
        if k == k' then
          Some x
        else
          loop k s
    | Skip s -> loop k s
    | Done -> None
  loop k start

let find p (Stream (step, start)) =
  let rec loop s =
    match step s with
    | Yield (s, x) ->
        if p x then
          Some x
        else
          loop s
    | Skip s -> loop s
    | Done -> None
  loop start

let zip (xs : stream _) (ys : stream _) = (,) <$> xs <*> ys
