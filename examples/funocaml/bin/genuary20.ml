open Claudius

let boot s =
  let w, h = Screen.dimensions s in
  let fb = Framebuffer.init (w, h) (fun _ _ -> 254) in
  let xoff = (w - 200) / 2
  and yoff = (h - 200) / 2 in
  Framebuffer.filled_rect xoff yoff 200 200 0 fb;
  fb

let tick t s fb _i =
  let w, h = Screen.dimensions s in
  let xoff = (w - 200) / 2
  and yoff = (h - 200) / 2 in
  let special = ((t mod 200) != 0) in
  Framebuffer.shader_inplace (fun p ->
    match p with
    | 0 -> 0
    | 254 -> 254
    | 255 -> if special then 255 else 253
    | p -> p - 1
  ) fb;
  match (Screen.font s) with
  | None -> fb;
  | Some (font) -> (
    let x = Random.int 8 and y = Random.int 8 in
    let ch = char_of_int (Random.int 256) in
    let col = if (((t / 200) mod 8) == x) then 255 else 253 in
    let _ = Framebuffer.draw_char (xoff + (x * 22) + 20) (yoff + (y * 22) + 15) font ch col fb in ();
  ); fb

let palette = Palette.of_list (List.rev (0xff0000 :: (Palette.to_list (Palette.generate_mono_palette 255))))

let slide = (palette, Some boot, tick)
