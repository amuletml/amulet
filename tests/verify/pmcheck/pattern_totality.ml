type option 'a = None | Some of 'a

module Constructors =
  (* Complete pattern *)
  let _ = function
    | None -> ()
    | Some _ -> ()

  (* Incompleteness *)
  let f None = ()
  let f (Some _) = ()

  (*  Redundant pattern *)
  let _ = function
    | Some _ -> ()
    | Some _ -> ()
    | None -> ()

module GADT =
  type term 'a =
    | Int  : int -> term int
    | Unit : unit -> term unit
    | Any  : 'a -> term 'a

  (* Complete pattern *)
  let _ = function
    | Int _ -> ()
    | Any _ -> ()

  (* Complete pattern for all type variables *)
  let f : forall 'a. term 'a -> () = function
    | Int _ -> ()
    | Any _ -> ()
    | Unit _ -> ()

  (* Incomplete pattern *)
  let _ = function
    | Int _ -> ()

  (* Incomplete pattern for all type variables *)
  let f : forall 'a. term 'a -> () = function
    | Int _ -> ()

  (* Redundant pattern *)
  let _ = function
    | Int _ -> ()
    | Any _ -> ()
    | Any _ -> ()

module Records =
  (* Complete pattern *)
  let f { x = _ } = ()

  (* Incomplete pattern *)
  let _ = function
    | { x = 0 } -> ()
    | { y = 0 } -> ()

  (* Redundant pattern *)
  let _ = function
    | { x = _, y = _ } -> ()
    | { y = 0 } -> ()

module Tuples =
  (* Complete pattern *)
  let f (_, _) = ()

  (* Incomplete pattern *)
  let _ = function
    | (_, 0) -> ()
    | (0, _) -> ()

  let _ = function
    | Some _, Some _-> ()
    | None, None -> ()

  (* Redundant pattern *)
  let _ = function
    | (_, _) -> ()
    | (_, _) -> ()

module Literals =
  (* Complete *)
  let f () = ()
  let f = function
    | true -> ()
    | false -> ()
  let f = function
    | 0 -> ()
    | _ -> ()

  (* Incomplete *)
  let _ = function
    | true -> ()
  let _ = function
    | 0 -> ()

  (* Redundant *)
  let _ = function
    | () -> ()
    | _ -> ()
  let _ = function
    | true -> ()
    | false -> ()
    | _ -> ()
  let _ = function
    | _ -> ()
    | 0 -> ()
