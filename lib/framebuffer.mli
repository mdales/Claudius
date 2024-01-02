
type t = int array array

val init: int -> int -> (int -> int -> int) -> t

val filled_circle: int -> int -> float -> int -> t -> unit

val draw_line: int -> int -> int -> int -> int -> t -> unit

val pixel_write: int -> int -> int -> t -> unit

val pixel_read: int -> int -> t -> int option

val render: t -> Primatives.t list -> unit