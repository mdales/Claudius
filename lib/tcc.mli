
type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> Framebuffer.t

val run: string -> boot_func option -> tick_func -> Screen.t -> unit
