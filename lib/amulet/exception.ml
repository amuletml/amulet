open import "./base.ml"
open import "./option.ml"
open import "./typeable.ml"

type some 'constraint =
  MkSome : forall 'x. 'constraint 'x => 'x -> some 'constraint

deriving instance typeable some

class typeable 'e => exception 'e begin
  val into_exception : 'e -> some exception
  let into_exception e = MkSome e

  val from_exception : some exception -> option 'e
  let from_exception (MkSome x) = cast x

  val describe_exception : 'e -> string
end

deriving instance typeable exception

instance exception (some exception) begin
  let into_exception x = x
  let from_exception x = Some x
  let describe_exception (MkSome exc) = describe_exception exc
end

external private val prim_throw : (some exception -> string) -> some exception -> 'a =
  "function(desc, exc) return error(setmetatable(exc, { __tostring = desc })) end"
external private val prim_catch : (unit -> 'a) -> (some exception -> 'a) -> 'a =
  "function(k, h) \
     local ok, err = pcall(k) \
     if not ok then \
       return h(err) \
     else \
       return err \
     end \
   end"

let throw x = prim_throw describe_exception (into_exception x)

let catch computation handler =
  let handle exc =
    match from_exception exc with
    | Some x -> handler x
    | None -> throw exc
  prim_catch computation handle

let try_ computation =
  catch (fun _ -> Some (computation ())) (fun (_ : some exception) -> None)

type user_error = UserError of string
deriving instance typeable user_error

instance exception user_error begin
  let describe_exception (UserError x) = "User error: " ^ x
end

let error x = throw (UserError x)

type arithmetic_exception = DivideBy0
deriving instance typeable arithmetic_exception

instance exception arithmetic_exception begin
  let describe_exception DivideBy0 = "Arithmetic exception: division by zero"
end

type unwrap_error = Unwrap of string
deriving instance typeable unwrap_error

instance exception unwrap_error begin
  let describe_exception (Unwrap x) = "Tried to unwrap " ^ x ^ ", but there was nothing there!"
end

type invalid_arg = Invalid of string
deriving instance typeable invalid_arg

instance exception invalid_arg begin
  let describe_exception (Invalid x) = "Invalid argument to function " ^ x
end

let from_some x =
  match x with
  | Some v -> v
  | None -> throw (Unwrap "Some")
