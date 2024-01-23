(** Hello *)

type t

type glyph

val load_psf_font: string -> (t, string) result

val print_header: t -> unit

val glyph_count: t -> int

val glyph_of_char: t -> Uchar.t -> glyph option

val glyph_dimensions: glyph -> (int * int)

val glyph_bitmap: glyph -> bytes
