open Claudius

let palette =
  Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let draw_background s fb =
  let w, h = Screen.dimensions s in
  let horizon_height = 2 * (h / 3) in
    Framebuffer.filled_rect 0 0 w horizon_height 8 fb;
    Framebuffer.filled_rect 0 horizon_height 640 (h  - horizon_height) 12 fb

let draw_snow (t: int) (seed: int) (diameter: float) (fb: Framebuffer.t) =
  Random.init seed;
  for i = 0 to 480 do
    let updated_i =  (t mod 480) - i in
      let adjusted_updated_i =
        (if updated_i >= 0 then updated_i else updated_i + 480) in
          (* plot (Random.int 640) adjusted_updated_i; *)
          Framebuffer.filled_circle (Random.int 640) adjusted_updated_i diameter 12 fb
  done

let draw_tree x y fb =
  (* pot *)
  let pot_height = 70
  and pot_upper_radius = 50
  and pot_lower_radius = 30 in
  Framebuffer.filled_polygon [
    (x + pot_lower_radius, y);
    (x + pot_upper_radius, y - pot_height);
    (x - pot_upper_radius, y - pot_height);
    (x - pot_lower_radius, y)
  ] 2 fb;

  (* trunk *)
  let trunk_height = 50
  and trunk_radius = 20 in
  Framebuffer.filled_rect (x - trunk_radius) (y - (pot_height + trunk_height)) (trunk_radius * 2) (trunk_height) 3 fb;

  (* leaves *)
  let tree_height = 250
  and tiers = 4
  and tree_radius = 100 in
  for i = 0 to tiers do
  (* let i = 0 in *)
    Framebuffer.filled_polygon [
      ((x + tree_radius) + ((tiers - i) * 15), y - (pot_height + trunk_height + (i * 30)));
      (x, y - (pot_height + trunk_height + tree_height - ((tiers - i) * 30)));
      ((x - tree_radius) - ((tiers - i) * 15), y - (pot_height + trunk_height + (i * 30)))
    ] 6 fb
  done;

  (* star *)
  let star_offset = (y - (pot_height + trunk_height + tree_height)) in
  Framebuffer.filled_polygon [ (x, star_offset - 20); (x + 15, star_offset + 10); (x - 15, star_offset + 10) ] 4 fb;
  Framebuffer.filled_polygon [ (x, star_offset + 20); (x + 15, star_offset - 10); (x - 15, star_offset - 10) ] 4 fb


let tick t s fb i =

  let show_snow = Base.KeyCodeSet.exists (fun x -> x == 0x00000031) i in

  let w, h = Screen.dimensions s in
  draw_background s fb;
  if show_snow then draw_snow t 42 0.5 fb;
  draw_tree (w / 2) (h - 60) fb;
  if show_snow then draw_snow t 62 1.0 fb;
  fb

let slide = (palette, None, tick)
