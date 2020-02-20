external val ignore : 'a -> () = "function(x) end"

external val ( +  ) : int -> int -> int = "%int.add"
external val ( -  ) : int -> int -> int = "%int.sub"
external val ( *  ) : int -> int -> int = "%int.mul"
external val ( ** ) : int -> int -> int = "%int.pow"
external val ( /  ) : int -> int -> int = "%int.div"
external val ( == ) : int -> int -> bool = "%int.eq"
external val ( <= ) : int -> int -> bool = "%int.le"
external val ( <  ) : int -> int -> bool = "%int.lt"

external val ( +.  ) : float -> float -> float = "%float.add"
external val ( -.  ) : float -> float -> float = "%float.sub"
external val ( *.  ) : float -> float -> float = "%float.mul"
external val ( **. ) : float -> float -> float = "%float.pow"
external val ( /.  ) : float -> float -> float = "%float.div"
external val ( ==. ) : float -> float -> bool = "%float.eq"
external val ( <=. ) : float -> float -> bool = "%float.le"
external val ( <.  ) : float -> float -> bool = "%float.lt"

external val ( ^ ) : string -> string -> bool = "%string.concat"
external val ( ==? ) : string -> string -> bool = "%string.eq"
external val ( <=? ) : string -> string -> bool = "%string.le"
external val ( <?  ) : string -> string -> bool = "%string.lt"


let () = ignore (fun { i, s }->
  { int_add_c = 2 + 3
  , int_sub_c = 2 - 3
  , int_mul_c = 2 * 3
  , int_div_c = 6 / 2
  , int_exp_c = 2 ** 3
  , int_eq_c  = 2 == 3
  , int_lt_c  = 2 < 3
  , int_le_c  = 2 <= 3

  , int_add_l = i + 0
  , int_add_r = 0 + i
  , int_sub_l = i - 0
  , int_mul1_l = i * 1
  , int_mul1_r = 1 * i
  , int_mul_l = i * 0
  , int_mul_r = 0 * i
  , int_div_l = i / 1

  , float_add_c = 2.0 +. 3.0
  , float_sub_c = 2.0 -. 3.0
  , float_mul_c = 2.0 *. 3.0
  , float_div_c = 6.0 /. 2.0
  , float_exp_c = 2.0 **. 3.0
  , float_eq_c  = 2.0 ==. 3.0
  , float_lt_c  = 2.0 <. 3.0
  , float_le_c  = 2.0 <=. 3.0

  , str_con_c = "foo" ^ "bar"
  , str_eq_c = "foo" ==? "bar"
  , str_le_c = "foo" <=? "bar"
  , str_lt_c = "foo" <? "bar"

  , str_con_l = s ^ ""
  , str_con_r = "" ^ s

  , int_eq_u = i == i
  , str_eq_u = s ==? s
  })
