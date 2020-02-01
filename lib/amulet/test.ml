open import "prelude.ml"
open import "./colors.ml"

module Test = struct

  let run_test (name, test) =
    if test () then
      (Colors.bg_green # Colors.bold) "Test Passed" ^ ": " ^ name
      |> put_line
    else
      (Colors.bg_red # Colors.bold) "Test Failed" ^ ": " ^ name
      |> put_line

  let expect (name, test, expected) =
    let actual = test ()
    if actual == expected then
      (Colors.bg_green # Colors.bold) "Test Passed" ^ ": " ^ name
      |> put_line
    else
      (Colors.bg_red # Colors.bold) "Test Failed" ^ ": " ^ name ^
        "\n   Expected: " ^ Colors.bold (show expected) ^
        "\n     Actual: " ^ Colors.bold (show actual)
        |> put_line

  let rec run_tests tests =
    match tests with
    | Cons (hd, tl) -> run_test hd; run_tests tl
    | Nil -> ()

end
