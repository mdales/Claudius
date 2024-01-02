
type screen = {
  width           : int ;
  height          : int ;
  scale           : int ;
  palette         : Palette.t ;
}

type boot_func = screen -> Framebuffer.t
type tick_func = int -> screen -> Framebuffer.t -> Framebuffer.t

val tcc_init: screen -> string -> boot_func option -> tick_func -> unit
