(* Generates an SVG of the Amulet logo.

  See `assets/logo.svg' or `assets/logo.png' for an example of the output *)

let x |> f = f x

external val string_of_float : float -> string = "function(x) return (\"%g\"):format(x) end"
external val float_of_int : int -> float = "function(x) return x end"

external val io_write : string -> unit = "io.write"

class show 'a
  val show : 'a -> string

instance show float
  let show = string_of_float

let print x =
  io_write (show x)
  io_write "\n"

module Math =
  external val sin : float -> float = "math.sin"
  external val cos : float -> float = "math.cos"
  external val rad : float -> float = "math.rad"

  (* Convert polar coordinates to Cartesian ones *)
  let polar r theta = (r *. cos theta, r *. sin theta)

module List =
  type list 'a =
    | Nil
    | Cons of 'a * list 'a

  let x :: xs = Cons (x, xs)

  let map f = function
    | Nil -> Nil
    | Cons (x, xs) -> f x :: map f xs

  let foldl f a = function
    | Nil -> a
    | Cons (x, xs) -> foldl f (f a x) xs

  let range = function
    | x when x < 0 -> Nil
    | x -> x :: range (x - 1)

  let xs ++ ys =
    match xs, ys with
    | Nil, ys -> ys
    | xs, Nil -> xs
    | Cons (x, xs), ys ->
       let go = function
         | Nil -> ys
         | Cons (x, xs) -> x :: go xs
       x :: go xs

open List

module SVG =
  type node =
    | ClosedPath of { style : string, points : list (float * float) }
    | Text of { style : string, text : string, position: (float * float ) }

  instance show node
    let show = function
      | ClosedPath { style, points } ->
         let points' = foldl (fun rest (x, y) -> rest ^ " " ^ show x ^ "," ^ show y) "" points
         "<path style=\"" ^ style ^ "\" d=\"M" ^ points' ^ " z\" />"
      | Text { style, text, position = (x, y) } ->
         "<text style=\"" ^ style ^ "\" x=\"" ^ show x ^ "\" y=\"" ^ show y ^ "\" text-anchor=\"middle\">" ^ text ^ "</text>"

  type svg = SVG of {
    viewbox : { x : float, y : float, width : float, height : float },
    elements : list node
  }

  instance show svg
    let show (SVG { viewbox = { x, y, width, height }, elements }) =
      "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\""
      ^ show x ^ " " ^ show y ^ " " ^ show width ^ " " ^ show height ^ "\">\n"
      ^ foldl (fun a x -> a ^ "  " ^ show x ^ "\n") "" elements
      ^ "</svg>"

open SVG

let () =
  let points = 5

  let core_size = 10.0 (* The size of the "core" shape *)
  let padding = 1.0    (* The padding between the core and the rest *)
  let edge_size = 2.0  (* The width of the edges *)

  let rot_offset = 0.0 -. 18.0 (* The angle (in degrees) to rotate the shape *)
  let gap = 5.0                (* The angle to inlay the edges by *)


  (* Some helper functions and values *)
  let angle = 360.0 /. float_of_int points
  let angle_at point = rot_offset +. angle *. float_of_int point
  let point_at r offset theta =
    angle_at theta |> (+.offset) |> Math.rad
    |> Math.polar r

  let core = range points |> map (point_at core_size 0.0)
  let edges =
    range points
    |> map (fun point -> ClosedPath {
              style = "fill:#216778",
              points = point_at (core_size +. padding)              gap          point
                    :: point_at (core_size +. padding +. edge_size) gap          point
                    :: point_at (core_size +. padding +. edge_size) (0.0 -. gap) (point + 1)
                    :: point_at (core_size +. padding)              (0.0 -. gap) (point + 1)
                    :: Nil
           })

  print (SVG {
    viewbox = { x = 0.0 -. 13.0, y = 0.0 -. 14.0, width = 26.0, height = 26.0 },
    elements = ClosedPath { style = "fill:#5fbcd3", points = core }
           :: edges
           ++ (Text {
                style = "font-size:15px;font-family:sans-serif;fill:#ffffff;",
                position = (0.0, 6.0),
                text = "∀" } :: Nil)
  })
