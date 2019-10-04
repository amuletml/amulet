type nat = Z | S of nat

type function (+) ('a : nat) ('b : nat) : nat begin
  Z + 'a = 'a
  (S 'x) + 'y = S ('x + 'y)
end

type pi 'number =
  | SZ : pi Z
  | SS : pi 'n -> pi (S 'n)

type 'a ~~ 'b =
  Refl : 'a ~ 'b => 'a ~~ 'b

let add_zero_right (x : pi 'n) : ('n + Z) ~~ 'n =
  match x with
  | SZ -> Refl
  | SS k -> match (add_zero_right k) with | Refl -> Refl

let add_succ_right (x : pi 'n) (y : pi 'm) : ('n + S 'm) ~~ S ('n + 'm) =
  match x, y with
  | SZ, _ -> Refl
  | SS k, m ->
      match (add_succ_right k m) with
      | Refl -> Refl
