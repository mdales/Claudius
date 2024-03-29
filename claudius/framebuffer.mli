
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
    but just some basics to let people get up and running. Shapes will be automatically clipped to fit
    the framebuffer. Position (0, 0) is in the top left of the screen. *)

val draw_line: int -> int -> int -> int -> int -> t -> unit
(** [draw_line x0 y0 x1 y1 colour framebuffer] Draws a line between ([x0], [y0]) ([x1], [y1]) specified in
    the specified [colour] into [framebuffer]. *)

val draw_circle: int -> int -> float -> int -> t -> unit
(** [draw_circle x0 y0 radius colour framebuffer] Draws the outline of a circle centred at ([x0], [y0]) with the specified [radius]
    in the specified [colour] into [framebuffer]. *)

val filled_circle: int -> int -> float -> int -> t -> unit
(** [filled_circle x0 y0 radius colour framebuffer] Draws a filled circle centred at ([x0], [y0]) with the specified [radius]
    in the specified [colour] into [framebuffer]. *)

val draw_rect: int -> int -> int -> int -> int -> t -> unit
(** [draw_rect x y width height colour framebuffer] Draws the outline of a rectangle aligned with the window, with the top left at ([x], [y]) and size of ([width], [height])
    in the specified [colour] into [framebuffer]. *)

val filled_rect: int -> int -> int -> int -> int -> t -> unit
(** [filled_rect x y width height colour framebuffer] Draws a filled rectangle aligned with the window, with the top left at ([x], [y]) and size of ([width], [height])
    in the specified [colour] into [framebuffer]. *)

val draw_triangle: int -> int -> int -> int -> int -> int -> int -> t -> unit
(** [draw_triangle x0 y0 x1 y1 x2 y2 colour framebuffer] Draws the outline of a triangle made from the points ([x0], [y0]), ([x1], [y1]), and ([x2], [y2])
    in the specified [colour] into [framebuffer]. *)

val filled_triangle: int -> int -> int -> int -> int -> int -> int -> t -> unit
(** [filled_triangle x0 y0 x1 y1 x2 y2 colour framebuffer] Draws a filled triangle made from the points ([x0], [y0]), ([x1], [y1]), and ([x2], [y2])
    in the specified [colour] into [framebuffer]. *)

val draw_polygon: (int * int) list -> int -> t -> unit
(** [draw_polygon points colour framebuffer] Draws the outline of a polygon made from the list of [points]
    in the specified [colour] into [framebuffer]. *)

val filled_polygon: (int * int) list -> int -> t -> unit
(** [filled_polygon points colour framebuffer] Draws a filled polygon made from the list of [points]
    in the specified [colour] into [framebuffer]. *)

(** {1 Text rendering operations} *)

val draw_char: int -> int -> Font.t -> char -> int -> t -> int
(** [draw_char x y font c colour framebuffer] Draws a single character [c] in the specified [colour] using [font]. The top left of 
    the charcter is the point specified by position ([x], [y]). The return value is the number of pixels wide the character was. *)

val draw_string: int -> int -> Font.t -> string -> int -> t -> int
(** [draw_string x y font s colour framebuffer] Draws the string [s] in the specified [colour] using [font]. The top left of 
    the first charcter is the point specified by position ([x], [y]). The return value is the number of pixels wide the string was. *)

(** {1 Framebuffer wide operations} *)

type shader_func = int -> int

type shaderi_func = int -> int -> t -> int

val shader: shader_func -> t -> t
(** [shader f framebuffer] Generates a new framebuffer of the same dimensions by applying the provided 
    function [f] to each pixel value in the original to generate a new pixel in the target. *)

val shaderi: shaderi_func -> t -> t
(** [shader f framebuffer] Generates a new framebuffer of the same dimensions by applying the provided 
    function [f] to each pixel value and its coordinates in the original to generate a new pixel in the target. *)

val shader_inplace: shader_func -> t -> unit
(** [shader f framebuffer] Updates a framebuffer by applying the provided 
    function [f] to each pixel value to update its value. *)

val shaderi_inplace: shaderi_func -> t -> unit
(** [shader f framebuffer] Updates a framebuffer by applying the provided 
    function [f] to each pixel value and its coordinate value to update its value. *)

val merge: (int -> int -> int) -> t -> t -> t
(** [merge f first second] Takes two framebuffers of equal size and applys the function [f] to each pixel 
    pair in turn to generate a new framebuffer. *)

val merge_inplace: (int -> int -> int) -> t -> t -> unit
(** [merge_inplase f first second] Takes two framebuffers of equal size and applys the function [f] to each pixel 
    pair in storing the result back in the first provided framebuffer. *)


(** {1 Misc} *)

val render: t -> Primitives.t list -> unit
(** [render framebuffer primitives] Takes a list of primative shapes and calls the appropriate
    drawing operation for each in turn to render them into the provide framebuffer. *)

val to_array: t -> int array array
(** [to_array framebuffer] converts the framebuffer into a 2D array. The top level array is an array
    of rows, and each row is an array of palette entry colours. *)
