(**
  Checks exponential behaviour of the inhabitation checker.

  Note: putting a type variable in all of these will still cause issues. We just
  rely on the fact that you'll have concrete boundaries somewhere. I'm not smart
  enough to do this correctly.
*)

type t0 = T0 of int
type t1 = T1 of t0 * t0
type t2 = T2 of t1 * t1
type t3 = T3 of t2 * t2
type t4 = T4 of t3 * t3
type t5 = T5 of t4 * t4
type t6 = T6 of t5 * t5
type t7 = T7 of t6 * t6
type t8 = T8 of t7 * t7
type t9 = T9 of t8 * t8
type t10 = T10 of t9 * t9
type t11 = T11 of t10 * t10
type t12 = T12 of t11 * t11
type t13 = T13 of t12 * t12
type t14 = T14 of t13 * t13
type t15 = T15 of t14 * t14
type t16 = T16 of t15 * t15
type t17 = T17 of t16 * t16
type t18 = T18 of t17 * t17
type t19 = T19 of t18 * t18
type t20 = T20 of t19 * t19
type t21 = T21 of t20 * t20
type t22 = T22 of t21 * t21
type t23 = T23 of t22 * t22
type t24 = T24 of t23 * t23
type t25 = T25 of t24 * t24
type t26 = T26 of t25 * t25
type t27 = T27 of t26 * t26
type t28 = T28 of t27 * t27
type t29 = T29 of t28 * t28
type t30 = T30 of t29 * t29
type t31 = T31 of t30 * t30
type t32 = T32 of t31 * t31
type t33 = T33 of t32 * t32
type t34 = T34 of t33 * t33
type t35 = T35 of t34 * t34
type t36 = T36 of t35 * t35
type t37 = T37 of t36 * t36
type t38 = T38 of t37 * t37
type t39 = T39 of t38 * t38
type t40 = T40 of t39 * t39
type t41 = T41 of t40 * t40
type t42 = T42 of t41 * t41
type t43 = T43 of t42 * t42
type t44 = T44 of t43 * t43
type t45 = T45 of t44 * t44
type t46 = T46 of t45 * t45
type t47 = T47 of t46 * t46
type t48 = T48 of t47 * t47
type t49 = T49 of t48 * t48
type t50 = T50 of t49 * t49

let f (T50 _) = ()
