external val bottom : 'a -> () = "nil"
let () = bottom (fun { i, s, u, fn, x }->
  { int_add_c = 2 + 3
  , int_sub_c = 2 - 3
  , int_mul_c = 2 * 3
  , int_div_c = 6 / 2
  , int_exp_c = 2 ** 3
  , int_eq_c  = 2 == 3
  , int_ne_c  = 2 <> 3
  , int_lt_c  = 2 < 3
  , int_gt_c  = 2 > 3
  , int_le_c  = 2 >= 3
  , int_ge_c  = 2 <= 3

  , int_add_l = i + 0
  , int_add_r = 0 + i
  , int_sub_l = i - 0
  , int_mul1_l = i * 1
  , int_mul1_r = 1 * i
  , int_mul_l = i * 0
  , int_mul_r = 0 * i
  , int_div_l = i / 1

  , str_con_c = "foo" ^ "bar"
  , str_eq_c = "foo" == "bar"
  , str_ne_c = "foo" <> "bar"

  , str_con_l = s ^ ""
  , str_con_r = "" ^ s

  , bol_eq_c = true == false
  , bol_ne_c = true <> false

  , uni_eq = () == u
  , uni_ne = () <> u

  , app = fn @@ x

  , int_eq_u = i == i
  , int_ne_u = i <> i
  , str_eq_u = s == s
  , str_nq_u = s <> s
  })
