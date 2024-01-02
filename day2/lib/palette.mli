
type t

exception String_not_multiple_of_chunk_size

val generate_mono_palette: int -> t

val generate_plasma_palette: int -> t

val load_tic80_palette: string -> t

val size: t -> int

val index_to_rgb: t -> int -> int option

val to_list: t -> int list

val from_list: int list -> t