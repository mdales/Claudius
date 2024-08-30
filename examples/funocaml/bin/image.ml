open Claudius
open Pcxlib

let tick pcx _t s p _i =
  let sw, sh = Screen.dimensions s in
  let w, h = Pcx.dimensions pcx in
  let xoff = (sw - w) / 2
  and yoff = (sh - h) / 2 in
  for x = 0 to (w - 1) do
    for y = 0 to (h - 1) do
      let v = Pcx.read_pixel pcx x y in
      Framebuffer.pixel_write (x + xoff) (y + yoff) v p
    done
  done;
  p

let generate_slide pcxfile =

  let pcx = Result.get_ok (Pcx.load pcxfile) in

  let palette = match Pcx.palette pcx with
    | None -> Palette.generate_mono_palette 256
    | Some pal -> (
      Palette.of_list (
        List.map (fun (r, g, b) ->
          r + (g * 256) + (b * 256 * 256)
        ) (Array.to_list pal)
      )
    )
    in

  (palette, None, tick pcx)
