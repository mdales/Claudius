
type t = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : Palette.t ;
}

let create (width : int) (height : int) (scale : int) (palette : Palette.t) : t =
  { width ; height ; scale ; palette }

let dimensions (screen : t) : int * int =
  screen.width, screen.height

let palette (screen : t) : Palette.t =
  screen.palette

let scale (screen : t) : int =
  screen.scale