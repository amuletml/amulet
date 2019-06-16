type nat = Z | S of nat
type snat 'n =
  | SZ : snat Z
  | SS : snat 'k -> snat (S 'k)
type fin 'n =
  | FZ : fin (S 'k)
  | FS : fin 'k -> fin (S 'k)

let to_fin (x : snat 'n) : fin 'n =
  match x with
  | SS SZ -> FS FZ
  | SS x -> FS (to_fin x)
  | SZ -> to_fin SZ
