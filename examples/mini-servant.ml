external val (==) : 'a -> 'a -> bool = ""
external val error : string -> 'a = "error"

type 'a :> 'b
external val encode_int : int -> string = "tostring"
external val parse_int : (int -> 'f int) -> 'f int -> string -> 'f int =
  "function(some, none, x) \
    if tonumber(x) then \
      return some(tonumber(x)) \
    else \
      return none \
    end \
  end"
external val print : string -> unit = "print"

type get 'a
type 'a <|> 'b
let x :<|> y = (x, y)
type capture ('x : type)
type p ('x : string)

type proxy 'a = Proxy

type option 'a = Some of 'a | None

class encode 'a begin
  val encode : 'a -> string
end

instance encode int begin
  let encode = encode_int
end

instance encode string begin
  let encode x = x
end

class parse 'a begin
  val parse : string -> option 'a
end

instance parse int begin
  let parse = parse_int Some None
end

let f <<< g = fun x -> f (g x)

class server 'layout 'server | 'layout -> 'server begin
  val route : proxy 'layout -> 'server -> list string -> option (unit -> string)
end

instance encode 'a => server (get 'a) (unit -> 'a) begin
  let route _ handler = function
    | [] -> Some (encode <<< handler)
    | _ -> None
end

instance (server 'a 'r * server 'b 's) => server ('a <|> 'b) ('r * 's) begin
  let route _ (ha, hb) xs =
    match route (Proxy : proxy 'a) ha xs with
    | Some p -> Some p
    | None -> route (Proxy : proxy 'b) hb xs
end

instance (Amc.known_string 'x * server 'r 's) => server (p 'x :> 'r) 's begin
  let route _ h = function
    | Cons (x, xs) ->
        if Amc.string_value @'x == x then
          route (Proxy : proxy 'r) h xs
        else None
    | [] -> None
end

instance (parse 'x * server 'r 's) => server (capture 'x :> 'r) ('x -> 's) begin
  let route _ h = function
    | Cons (x, xs) ->
        match parse x with
        | Some x -> route (Proxy : proxy 'r) (h x) xs
        | _ -> None
    | _ -> None
end

let serve p h xs =
  match route p h xs with
  | None -> error "404"
  | Some x -> x ()

type api <- (p "hello" :> get string) <|> (p "echo" :> capture int :> get int)
let handler : (unit -> string) * (int -> unit -> int) =
  (fun () -> "hello, world!") :<|> (fun x () -> x)

let x = serve (Proxy : proxy api) handler [ "hello" ]
let y = serve (Proxy : proxy api) handler [ "echo", "123" ]
