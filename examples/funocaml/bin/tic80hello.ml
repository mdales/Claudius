open Claudius
open Pcxlib

let palette =  Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let tick sprite t s _p _i =
  let sw, sh = 320, 240 in
  let w, h = Pcx.dimensions sprite in
  let xoff = (sw - (w * 1)) / 2
  and yoff = ((sh - (h * 1)) / 2) - 25 in

  let sfb = Framebuffer.init (sw, sh) (fun _ _ -> 13) in

  for x = 0 to (w - 1) do
    for y = 0 to (h - 1) do
          let v = Pcx.read_pixel sprite x y in
          let col = match v with
            | 2 -> 0
            | 3 -> 9
            | 4 -> 10
            | 6 -> 12
            | _ -> 13
          in
          Framebuffer.pixel_write ((1 * x) + xoff) ((y * 1) + yoff) col sfb
    done
  done ;

  if ((t / 90) mod 2) == 1 then Framebuffer.filled_rect 142 (yoff + 20) 24 3 12 sfb;

  let font = Option.get (Screen.font s) in
  ignore(Framebuffer.draw_string 100 140 font "HELLO WORLD!" 12 sfb);

  Framebuffer.init (Screen.dimensions s) (fun x y ->
    match Framebuffer.pixel_read (x / 2) (y / 2) sfb with
    | None -> 0
    | Some x -> x
  )


let generate_slide () =
  let sprite = Result.get_ok (Pcx.load "examples/funocaml/resources/tic80-sprite.pcx") in

  (palette, None, tick sprite)
