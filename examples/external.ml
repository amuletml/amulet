(** Convert a an integer to a string. *)
external val string_of_int : int -> string = "tostring"

module External =
  (** A "type token" - can hold any type *)
  type token 'a = Token

  (** A shorter alias for {!Token} *)
  let t = Token

  (** A function type, which can be provided by the FFI. *)
  type arrow 't =
    | Arg : (token 'a * arrow 'b) -> arrow ('a -> 'b)
    | Ret : token 'a -> arrow 'a

  (** A more convenient way of constructing arrows. *)
  let a @-> r = Arg (a, r)

  (** Evaluate an expression with the given type. *)
  external private val eval : string -> token 'a -> 'a = "function(x, _) return assert(load(\"return \" .. x))() end"

  (** Generate an external wrapper. Declared outside just to trick the inliner *)
  let private extern_wrapper : forall 'a. int -> string -> string -> arrow 'a -> string = fun i body args t ->
    match t with
    | Ret _ -> body ^ "(" ^ args ^ ")"
    | Arg (_, r) ->
       "function(x" ^ string_of_int i ^ ")"
       ^ " return " ^ extern_wrapper (i + 1) body (args ^ ", x" ^ string_of_int i) r
       ^ " end"

  (** Declare an external function with the specific type. *)
  let extern body (witness : arrow 'a) : 'a =
    match witness with
    | Ret _ -> eval body t
    | Arg (_, r) ->
       let contents = "function(x0) return " ^ extern_wrapper 1 body "x0" r ^ " end"
       eval contents t

  type foreign
  external val magic : foreign -> token 'a -> 'a = "function(x, _) return x end"
  external val is_nil : 'a -> bool = "function(x) return x == nil end"

module IO =
  open External
  let private lines' : string -> unit -> foreign = extern "io.lines" (t @-> Ret t)

  (** Apply a function [f] to every line in file [f] *)
  let lines f file =
    let iter = lines' file
    let go () =
      let line = iter ()
      if is_nil line then () else
      f (magic line t)
      go ()
   go ()

  (** Print the given line to stdout. *)
  let print : string -> unit = extern "print" (t @-> Ret t)

let () = IO.lines IO.print "external.ml"
