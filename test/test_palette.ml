open Claudius
open OUnit2

let test_basic_palette_of_ints _ =
  let cols = [0x000000 ; 0xFF0000 ; 0x00FF00 ; 0x0000FF ; 0xFFFFFF] in
  let pal = Palette.of_list cols in
  assert_equal ~msg:"Palette size" (List.length cols) (Palette.size pal);
  List.iteri (fun i c ->
    let v = Palette.index_to_rgb pal i in
    assert_equal ~msg:"Colour match" (Int32.of_int c) v
  ) cols;
  let rev = Palette.to_list pal in
  assert_equal ~msg:"Back to ints" cols rev

let test_plasma_palette_creation _ =
  let pal = Palette.generate_plasma_palette 16 in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  (* weak sauce perhaps, but just check there's no black/white here, i.e., not a mono palette *)
  List.iter (fun c ->
    assert_bool "Colour not black" (0x000000 != c);
    assert_bool "Colour not white" (0xFFFFFF != c);
  ) (Palette.to_list pal)

let test_mono_palette_creation _ =
  let pal = Palette.generate_mono_palette 16 in
  assert_equal ~msg:"Palette size" 16 (Palette.size pal);
  assert_equal ~msg:"Start with black" Int32.zero (Palette.index_to_rgb pal 0);
  assert_equal ~msg:"Wrap around to black" Int32.zero (Palette.index_to_rgb pal 16);
  (* I originally tested that we ended on white, but due to rounding errors we might be slightly off *)
  List.iter (fun c ->
    let r = c land 0xFF
    and g = (c lsr 8) land 0xFF
    and b = (c lsr 16) land 0xFF in
    assert_equal ~msg:"R equals G" r g;
    assert_equal ~msg:"R equals B" r b;
  ) (Palette.to_list pal)

let test_create_empty_palette_from_list _ =
  assert_raises Palette.ZeroEntryPalette (fun _ -> Palette.of_list [])

let test_create_empty_plasma _ =
  assert_raises Palette.ZeroEntryPalette (fun _ -> Palette.generate_plasma_palette 0);
  assert_raises Palette.ZeroEntryPalette (fun _ -> Palette.generate_plasma_palette (-1))

let test_create_empty_mono _ =
  assert_raises Palette.ZeroEntryPalette (fun _ -> Palette.generate_mono_palette 0);
  assert_raises Palette.ZeroEntryPalette (fun _ -> Palette.generate_mono_palette (-1))

let suite =
  "PaletteTests" >::: [
    "Test simple palette set up" >:: test_basic_palette_of_ints ;
    "Test plasma palette creation" >:: test_plasma_palette_creation ;
    "Test mono creation" >:: test_mono_palette_creation ;
    "Test fail to make empty palette" >:: test_create_empty_palette_from_list ;
    "Test fail to make zero entry plasma palette" >:: test_create_empty_plasma ;
    "Test fail to make zero entry mono palette" >:: test_create_empty_mono ;
  ]

let () =
  run_test_tt_main suite
