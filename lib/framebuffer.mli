
type t

type shader_func = int -> int

type shaderi_func = int -> int -> t -> int

val init: int * int -> (int -> int -> int) -> t

val filled_circle: int -> int -> float -> int -> t -> unit

val draw_line: int -> int -> int -> int -> int -> t -> unit

val draw_rect: int -> int -> int -> int -> int -> t -> unit

val filled_rect: int -> int -> int -> int -> int -> t -> unit

val draw_polygon: (int * int) list -> int -> t -> unit

val pixel_write: int -> int -> int -> t -> unit

val pixel_read: int -> int -> t -> int option

val shader: shader_func -> t -> t

val shaderi: shaderi_func -> t -> t

val shader_inplace: shader_func -> t -> unit

val shaderi_inplace: shaderi_func -> t -> unit

val render: t -> Primitives.t list -> unit

val to_array: t -> int array array