open Claudius
open OUnit2

let test_basic_framebuffer_creation _ =
  let fb = Framebuffer.init (10, 20) (fun x y -> x + (y * 10)) in
  let raw = Framebuffer.to_array fb in
  assert_equal ~msg:"Y axis size" 20 (Array.length raw);
  Array.iteri (fun y row ->
    assert_equal ~msg:"X axis size" 10 (Array.length row);
    Array.iteri (fun x pixel ->
      assert_equal ~msg:"Pixel value" (x + (y * 10)) pixel
    ) row
  ) raw


let test_fail_invalid_dimensions _ =
  assert_raises (Invalid_argument "Invalid width") (fun _ -> Framebuffer.init (-10, 20) (fun _ _ -> 0));
  assert_raises (Invalid_argument "Invalid height") (fun _ -> Framebuffer.init (10, -20) (fun _ _ -> 0))

let suite =
  "Frambuffer tests" >::: [
    "Test simple framebuffer set up" >:: test_basic_framebuffer_creation ;
    "Test fail with invalid dimensions" >:: test_fail_invalid_dimensions ;
  ]

let () =
  run_test_tt_main suite
