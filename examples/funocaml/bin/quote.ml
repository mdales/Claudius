open Claudius

let prose = "\"The enemy of art is the absence of limitations.\""
let author = "Orson Wells"

let prose_font = Result.get_ok (
  Bdfparser.Bdf.create "/Users/michael/dev/classic-mac-fonts/bdf/Times-24.bdf"
)
let author_font = Result.get_ok (
  Bdfparser.Bdf.create "/Users/michael/dev/classic-mac-fonts/bdf/Times-14.bdf"
)

let tick t s _p _i =
  let w, h = Screen.dimensions s in
  let fb = Framebuffer.init (w, h) (fun _ _ -> ((t / 100) mod 16)) in
  let w = Textslide.draw_string 60 (h / 2) prose_font prose (((t / 100) + 8) mod 16) fb in
  ignore(Textslide.draw_string (w - 10) ((h / 2) + 20) author_font author (((t / 100) + 8) mod 16) fb);
  fb


let palette = Palette.generate_plasma_palette 16

let slide = (palette, None, tick)
