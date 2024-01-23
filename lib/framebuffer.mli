
(** Provides the simulated framebuffer for Claudius.

The framebuffer is logically an array of memory in which you draw
using palette entries. *)

type t

(** {1 Initializations} *)

val init: int * int -> (int -> int -> int) -> t
(** [init width height f] Creates a new framebuffer of the specified size [width] x [height] and
    initialises each pixel using the provided function. The function is provided the x and y
    coordinates of the pixel and should return the colour there. *)


(** {1 Single pixel operations} *)

val pixel_write: int -> int -> int -> t -> unit
(** [pixel_write x y colour framebuffer] Set the pixel at the specified coordinate to palette colour
    in the provided framebuffer. If the coordinate is outside the framebuffer then nothing is drawn, but
    there is no error. *)

val pixel_read: int -> int -> t -> int option
(** [pixel_read x y framebuffer] Get the pixel colour at the specified coordinate in the
    provided framebuffer. If the coordinate is outside the framebuffer you get None,
    otherwise Some colour. *)


(** {1 Drawing operations}
    These operations let you modify the framebuffer with basic shapes. This list isn't exhaustive,
    but just some basics to let people get up and running. Inspired by the primatives from TIC80. *)

val draw_line: int -> int -> int -> int -> int -> t -> unit
(** [draw_line x0 y0 x1 y1 colour framebuffer] Draws a line between the two points specified in
    the specified colour. *)

val draw_circle: int -> int -> float -> int -> t -> unit

val filled_circle: int -> int -> float -> int -> t -> unit

val draw_rect: int -> int -> int -> int -> int -> t -> unit

val filled_rect: int -> int -> int -> int -> int -> t -> unit

val draw_polygon: (int * int) list -> int -> t -> unit

val draw_char: int -> int -> Font.t -> char -> int -> t -> int

val draw_string: int -> int -> Font.t -> string -> int -> t -> int

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