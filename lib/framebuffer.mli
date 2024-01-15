
(** Provides the simulated framebuffer for Claudius.
    
The framebuffer is logically an array of memory in which you draw
using palette entries. *)

type t

(** {1 Initializations} *)

val init: int * int -> (int -> int -> int) -> t
(** [init width height f] Creates a new framebuffer of the specified size [width] x [height] and 
    initialises each pixel using the provided function. The function is provided the x and y 
    coordinates of the pixel and should return the colour there. *)


(** {1 Drawing operations} *)

val filled_circle: int -> int -> float -> int -> t -> unit

val draw_line: int -> int -> int -> int -> int -> t -> unit

val draw_rect: int -> int -> int -> int -> int -> t -> unit

val filled_rect: int -> int -> int -> int -> int -> t -> unit

val draw_polygon: (int * int) list -> int -> t -> unit

(** {1 Single pixel operations} *)

val pixel_write: int -> int -> int -> t -> unit

val pixel_read: int -> int -> t -> int option

(** {1 Framebuffer wide operations} *)

type shader_func = int -> int

type shaderi_func = int -> int -> t -> int

val shader: shader_func -> t -> t

val shaderi: shaderi_func -> t -> t

val shader_inplace: shader_func -> t -> unit

val shaderi_inplace: shaderi_func -> t -> unit

(** {1 Misc} *)

val render: t -> Primitives.t list -> unit
(** [render framebuffer primitives] Takes a list of primative shapes and calls the appropriate
    drawing operation for each in turn to render them into the provide framebuffer. *)

val to_array: t -> int array array
(** [to_array framebuffer] converts the framebuffer into a 2D array. The top level array is an array
    of rows, and each row is an array of palette entry colours. *)