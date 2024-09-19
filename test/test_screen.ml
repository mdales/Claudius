open Claudius
open OUnit2

let test_basic_screen_creation _ =
  let palette = Palette.generate_mono_palette 2 in
  let screen = Screen.create 640 480 2 palette in
  assert_equal ~msg:"Dimensions" (640, 480) (Screen.dimensions screen);
  assert_equal ~msg:"Scale" 2 (Screen.scale screen);
  assert_equal ~msg:"Font" None (Screen.font screen);
  assert_equal ~msg:"Palette" palette (Screen.palette screen)

let suite =
  "Screen tests" >::: [
    "Test simple screen set up" >:: test_basic_screen_creation ;
  ]

let () =
  run_test_tt_main suite
