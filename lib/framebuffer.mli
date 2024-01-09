
type t

type shader_func = int -> int

val init: int * int -> (int -> int -> int) -> t

val filled_circle: int -> int -> float -> int -> t -> unit

val draw_line: int -> int -> int -> int -> int -> t -> unit

val pixel_write: int -> int -> int -> t -> unit

val pixel_read: int -> int -> t -> int option

val shader: shader_func -> t -> t

val render: t -> Primitives.t list -> unit

val to_array: t -> int array array