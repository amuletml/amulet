open import "./option.ml"
open import "./base.ml"

type name = Name of string * int

instance eq name begin
  let (Name (_, i)) == (Name (_, i')) = i == i'
end

instance ord name begin
  let (Name (_, i)) <= (Name (_, i')) = i <= i'
end

type literal =
  | True | False
  | Int of int | String of string | Float of float
  | Unit

instance eq literal begin
  let a == b =
    match a, b with
    | True, True -> true
    | False, False -> true
    | Int i, Int i' -> i == i'
    | String s, String s' -> s == s'
    | Float f, Float f' -> f == f'
    | Unit, Unit -> true
    | _, _ -> false
end

instance ord literal begin
  let a <= b =
    match a with
    | True -> false
    | False ->
        match b with
        | True -> true
        | _ -> false
    | Int i ->
        match b with
        | True -> true | False -> true
        | Int i' -> i <= i'
        | _ -> false
    | String s ->
        match b with
        | True -> true | False -> true | Int _ -> true
        | String s' -> s <= s'
        | _ -> false
    | Float f ->
        match b with
        | True -> true | False -> true | Int _ -> true | String _ -> true
        | Float f' -> f <=. f'
        | _ -> false
    | Unit -> true
end

type pattern =
  | Wild_p
  | Var_p of name
  | Con_p of name
  | Des_p of name * pattern
  | As_p  of pattern * name

instance eq pattern begin
  let a == b =
    match a, b with
    | Wild_p, Wild_p -> true
    | Var_p n, Var_p n' -> n == n'
    | Con_p n, Con_p n' -> n == n'
    | Des_p (n, p), Des_p (n', p') ->
        n == n' && p == p'
    | As_p (p, n), As_p (p', n') -> n == n' && p == p'
    | _, _ -> false
end

type arm 'e =
  | Arm of pattern * 'e
  | Guarded of pattern * 'e * 'e

instance eq 'e => eq (arm 'e) begin
  let a == b =
    match a, b with
    | Arm (p, e), Arm (p', e') -> p == p' && e == e'
    | Guarded (p, g, e), Guarded (p', g', e') -> p == p' && g == g' && e == e'
    | _, _ -> false
end

type visibility = Required | Specified | Inferred

instance eq visibility begin
  let a == b =
    match a, b with
    | Required, Required -> true
    | Specified, Specified -> true
    | Inferred, Inferred -> true
    | _, _ -> false
end

type type_ex =
  | Con_t of name
  | Var_t of name
  | Promoted_t of name
  | Arr_t of type_ex * type_ex
  | Qual_t of type_ex * type_ex
  | Forall_t of {
      var : name,
      kind : option type_ex,
      vis : visibility,
      body : type_ex
  }
  | Extension_t of type_ex * list (string * type_ex)
  | Record_t of list (string * type_ex)
  | Tuple_t of type_ex * type_ex
  | Pair_t of type_ex * type_ex
  | Op_t of {
      left : type_ex,
      operator : name,
      right : type_ex
  }

instance eq type_ex begin
  let a == b =
    match a, b with
    | Con_t n, Con_t n' -> n == n'
    | Var_t n, Var_t n' -> n == n'
    | Promoted_t n, Promoted_t n' -> n == n'
    | Arr_t a, Arr_t b -> a == b
    | Qual_t a, Qual_t b -> a == b
    | Forall_t { body = body, var = var, kind = kind, vis = vis }
      , Forall_t { body = body', var = var', kind = kind', vis = vis' } ->
        vis == vis' && var == var' && body == body' && kind == kind'
    | Extension_t (a, rs), Extension_t (b, rs') -> a == b && rs == rs'
    | Record_t rs, Record_t rs' -> rs == rs'
    | Tuple_t c, Tuple_t d -> c == d
    | Pair_t c, Pair_t d -> c == d
    | Op_t { left, operator, right }, Op_t { left = left', operator = operator', right = right' } ->
        operator == operator' && left == left' && right == right'
    | _, _ -> false
end

type binding 'e =
  | Var_b of name * 'e
  | Match_b of pattern * 'e

instance eq 'e => eq (binding 'e) begin
  let a == b =
    match a, b with
    | Var_b a, Var_b b -> a == b
    | Match_b a, Match_b b -> a == b
    | _, _ -> false
end

type stmt 'e =
  | Guard_st of 'e
  | Gen_st of pattern * 'e
  | Let_st of list (binding 'e)

instance eq 'e => eq (stmt 'e) begin
  let a == b =
    match a, b with
    | Guard_st a, Guard_st b -> a == b
    | Gen_st a, Gen_st b -> a == b
    | Let_st a, Let_st b -> a == b
    | _, _ -> false
end

type recursiveness = Rec | Not_rec

instance eq recursiveness begin
  let a == b =
    match a, b with
    | Rec, Rec -> true
    | Not_rec, Not_rec -> true
    | _, _ -> false
end

type mod_ex =
  | Ref_mod of name
  | Import_mod of string

instance eq mod_ex begin
  let a == b =
    match a, b with
    | Ref_mod a, Ref_mod b -> a == b
    | Import_mod a, Import_mod b -> a == b
    | _, _ -> false
end

type expr =
  | Ref of name
  | App of expr * expr
  | Let of recursiveness * list (binding expr) * expr
  | Lit of literal
  | If of expr * expr * expr
  | Fun of pattern * expr
  | Match of expr * list (arm expr)
  | Bin_op of expr * expr * expr
  | Ascription of expr * type_ex

  | Record of list (string * expr)
  | Update of expr * list (string * expr)
  | Access of expr * string

  | Left_s of expr * expr
  | Right_s of expr * expr
  | Access_s of string

  | Tuple of list expr
  | Tuple_s of list (option expr)

  | Ty_app of expr * type_ex
  | Lazy of expr

  | List of list expr
  | List_comp of expr * list (stmt expr)
  | Do of {
    bind_name : option name,
    statements : list (stmt expr)
  }
  | Begin of list expr

(* TODO: this definition is too big for amulet *)
(* instance eq expr begin *)
(*   let a == b = *)
(*     match a, b with *)
(*     | Ref a, Ref b -> a == b *)
(*     | App a, App b -> a == b *)
(*     | Let a, Let b -> a == b *)
(*     | Lit a, Lit b -> a == b *)
(*     | If a, If b -> a == b *)
(*     | Fun a, Fun b -> a == b *)
(*     | Match a, Match b -> a == b *)
(*     | Bin_op a, Bin_op b -> a == b *)
(*     | Ascription a, Ascription b -> a == b *)

(*     | Record a, Record b -> a == b *)
(*     | Update a, Update b -> a == b *)
(*     | Access a, Access b -> a == b *)

(*     | Left_s a, Left_s b -> a == b *)
(*     | Right_s a, Right_s b -> a == b *)
(*     | Access_s a, Access_s b -> a == b *)

(*     | Tuple a, Tuple b -> a == b *)
(*     | Tuple_s a, Tuple_s b -> a == b *)

(*     | Ty_app a, Ty_app b -> a == b *)
(*     | Lazy a, Lazy b -> a == b *)

(*     | List a, List b -> a == b *)
(*     | List_comp a, List_comp b -> a == b *)
(*     | Do { bind_name = b, statements = s }, Do { bind_name = b', statements = s' } -> *)
(*         b == b' && s == s' *)
(*     | Begin a, Begin b -> a == b *)
(*     | _, _ -> false *)
(* end *)

class lift 'e begin
  val lift : 'e -> expr
end

instance lift int begin
  let lift x = Lit (Int x)
end

instance lift string begin
  let lift x = Lit (String x)
end

instance lift float begin
  let lift x = Lit (Float x)
end

instance lift () begin
  let lift _ = Lit Unit
end

instance lift bool begin
  let lift x =
    Lit @@
      if x then
        True
      else
        False
end

instance lift expr begin
  let lift x = x
end
