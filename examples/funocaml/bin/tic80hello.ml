open Claudius
open Giflib

let palette =  Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let tick (sprite : Image.t) t s _p _i =
  let sw, sh = 240, 136 in
  let w, h = Image.dimensions sprite in
  let xoff = (sw - (w * 1)) / 2
  and yoff = ((sh - (h * 1)) / 2) - 15 in

  let sfb = Framebuffer.init (sw, sh) (fun _ _ -> 13) in

  let pixels = Image.pixels sprite in
  for x = 0 to (w - 1) do
    for y = 0 to (h - 1) do
      let v = pixels.(x +  (y *w)) in
          let col = match v with
            | 2 -> 0
            | 3 -> 9
            | 4 -> 10
            | 5 -> 12
            | 6 -> 12
            | _ -> 13
          in
          Framebuffer.pixel_write ((1 * x) + xoff) ((y * 1) + yoff) col sfb
    done
  done ;

  if ((t / 90) mod 2) == 1 then Framebuffer.filled_rect ((sw / 2) - 16) (yoff + 20) 24 3 12 sfb;

  let font = Option.get (Screen.font s) in
  ignore(Framebuffer.draw_string 60 90 font "HELLO WORLD!" 12 sfb);

  let screen_w, screen_h = Screen.dimensions s in
  let xoff = (screen_w - (sw * 2)) / 2
  and yoff = (screen_h - (sh * 2)) / 2 in
  Framebuffer.init (screen_w, screen_h) (fun x y ->
    match Framebuffer.pixel_read ((x - xoff) / 2) ((y - yoff) / 2) sfb with
    | None -> 0
    | Some v -> v
  )


let generate_slide () =
  (* let sprite = Result.get_ok (Pcx.load "examples/funocaml/resources/tic80-sprite.pcx") in*)
  let sprite_file = GIF.from_file "examples/funocaml/resources/tic80-sprite.gif" in
  let sprite = GIF.get_image sprite_file 0 in


  (palette, None, tick sprite)
