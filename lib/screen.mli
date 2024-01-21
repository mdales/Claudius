(** Information about the display that can be accessed from the running code to work out screen size etc. *)

type t

val create: int -> int -> int -> Palette.t -> t

val dimensions: t -> int * int

val palette: t -> Palette.t

val scale : t -> int
