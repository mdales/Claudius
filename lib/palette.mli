(** Claudius works with colour palettes, as per computers of old. This module
lets you load and manipulate palettes. *)

type t

exception String_not_multiple_of_chunk_size


(** {1 Initializations} *)

val generate_mono_palette: int -> t

val generate_plasma_palette: int -> t

val load_tic80_palette: string -> t

(** {1 Conversion} *)

val to_list: t -> int list
(** [to_list palette] Converts the provided [palette] to a list of 24bpp RGB entries. *)

val of_list: int list -> t
(** [of_list list] Converts the provided [list] of 24bpp RGB entries to a palette. *)

(** {1 Access} *)

val size: t -> int
(** [size palette] Returns the number of entries in the palette. *)

val index_to_rgb: t -> int -> int32 option
