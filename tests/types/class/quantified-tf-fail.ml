class show 'a begin
  val show : 'a -> string
end
external val (^) : string -> string -> string = ""

class app 'f begin
  type apply 'a
end

type proxy 'a = Proxy

instance show (proxy 'a) begin
  let show _ = "Proxy"
end

instance app proxy begin
  type apply 'a = proxy 'a
end

type ex_ty_fun 'f =
  | Mk : forall 'f 'a. app 'f => apply 'f 'a -> ex_ty_fun 'f

instance (forall 'a. show (apply 'f 'a)) => show (ex_ty_fun 'f) begin
  let show (Mk x) = "Mk " ^ show x
end
