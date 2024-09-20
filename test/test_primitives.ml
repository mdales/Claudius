open Claudius
open OUnit2

let test_draw_line_direct _ =
  let fb = Framebuffer.init (10, 10) (fun _ _ -> 0) in
  assert_equal ~msg:"before" (Some 0) (Framebuffer.pixel_read 5 5 fb);
  Framebuffer.draw_line 3 3 7 7 1 fb;
  assert_equal ~msg:"before" (Some 1) (Framebuffer.pixel_read 5 5 fb)




let suite =
  "Primitives tests" >::: [
    "Test draw line direct" >:: test_draw_line_direct ;
  ]

let () =
  run_test_tt_main suite
