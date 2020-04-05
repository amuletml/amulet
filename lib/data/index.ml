open import "../prelude.ml"
private module String = import "../lua/string.ml"

class index 'a begin
  type key
  type value

  (** Look up an index, erroring it if is unavailable. *)
  val ( .() ) : 'a -> key 'a -> value 'a
  let ( .() ) xs k =
    match xs.?(k) with
    | Some x -> x
    | None -> Exception.(throw (Invalid ".()"))

  (** Look up an index, returning [None] if it is unavailable. *)
  val ( .?() ) : 'a -> key 'a -> option (value 'a)
end

instance index string begin
  type key = int
  type value = int

  let ( .?() ) str i =
    if i <= 1 || i > String.length str then None else
    String.char_code_at str i |> Some

  let ( .() ) str i =
    if i <= 1 || i > String.length str then
      let open Exception in
      throw (Invalid ("string.(): index " ^ show i ^ " is out of bounds"))
    else
      String.char_code_at str i
end

class index 'a => set_index 'a begin
  (** Create a copy of this object with the index added or modified. *)
  val ( .()<- ) : 'a -> key 'a -> value 'a -> 'a
end

class index 'a => mut_index 'a begin
  (** Mutate this object, adding/modifying an index. *)
  val ( .[]<- ) : 'a -> key 'a -> value 'a -> ()
end

let ( .[] ) : forall 'a. mut_index 'a => 'a -> key 'a -> value 'a = ( .() )

let ( .?[] ) : forall 'a. mut_index 'a => 'a -> key 'a -> option (value 'a) = ( .?() )
