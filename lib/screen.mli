type t

val create: int -> int -> int -> Palette.t -> t

val dimensions: t -> int * int

val palette: t -> Palette.t

val scale : t -> int
