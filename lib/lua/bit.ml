open import "amulet/base.ml"

external val arshift : int -> int -> int  = "bit32.arshift"
external val bitand  : int -> int -> int  = "bit32.band"
external val bitxor  : int -> int -> int  = "bit32.bxor"
external val bitnot  : int -> int         = "bit32.bnot"
external val bitor   : int -> int -> int  = "bit32.bor"
external val btest   : int -> int -> bool = "bit32.btest"

external val nth_bit : int -> int -> int = "bit32.extract"
external val set_bit : int -> int -> int -> int = "bit32.replace"

external val extract   : int -> int -> int -> int = "bit32.extract"
external val broadcast : int -> int -> int -> int -> int = "bit32.replace"

external val lrotate : int -> int -> int = "bit32.lrotate"
external val lshift  : int -> int -> int = "bit32.lshift"
external val rrotate : int -> int -> int = "bit32.rrotate"
external val rshift  : int -> int -> int = "bit32.rshift"

module Ops = begin
  let ( .<<. ) = lshift
  let ( .>>. ) = rshift
  let ( .|. )  = bitor
  let ( .&. )  = bitand
  let ( .^. )  = bitxor
end

let popcount i =
  let open Ops in
  let i = i - ((i .>>. 1) .&. 0x55555555)
  let i = (i .&. 0x33333333) + ((i .>>. 2) .&. 0x33333333)
  (((i + (i .>>. 4)) .&. 0x0F0F0F0F) * 0x01010101) .>>. 24

let bit n = Ops.( 1 .<<. n )
let x `with_bit` i = Ops.( x .|. bit i )
let x `without_bit` i = Ops.( x .|. bitnot (bit i) )
let x `test_bit` i = Ops.( (x .&. bit i) <> 0 )
