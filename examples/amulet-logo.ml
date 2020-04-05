(* Generates an SVG of the Amulet logo.

  See `assets/logo.svg' or `assets/logo.png' for an example of the output *)

open import "prelude.ml"

module Math =
  include import "lua/math.ml"

  (** Convert polar coordinates to Cartesian ones *)
  let polar r theta = (r *. cos theta, r *. sin theta)

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
         "<text style=\""
         ^ style
         ^ "\" x=\"" ^ show x ^ "\" y=\"" ^ show y ^ "\" text-anchor=\"middle\">" ^ text ^ "</text>"

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

  let core = point_at core_size 0.0 <$> [0 .. points]
  let edges =
    (fun point -> ClosedPath {
       style = "fill:#216778",
       points = point_at (core_size +. padding)              gap          point
             :: point_at (core_size +. padding +. edge_size) gap          point
             :: point_at (core_size +. padding +. edge_size) (0.0 -. gap) (point + 1)
             :: point_at (core_size +. padding)              (0.0 -. gap) (point + 1)
             :: Nil
     }) <$> [0 .. points]

  print (SVG {
    viewbox = { x = 0.0 -. 13.0, y = 0.0 -. 14.0, width = 26.0, height = 26.0 },
    elements = ClosedPath { style = "fill:#5fbcd3", points = core }
           :: edges
           ++ (Text {
                style = "font-size:15px;font-family:sans-serif;fill:#ffffff;",
                position = (0.0, 6.0),
                text = "∀" } :: Nil)
  })
