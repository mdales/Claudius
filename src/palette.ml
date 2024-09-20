type t = int32 array


let generate_mono_palette (size : int) : t =
  if size <= 0 then raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int): int32 ->
    let fi = float_of_int index and fsize = float_of_int size in
    let ch = ((fi /. fsize) *. 255.0) in
    Int32.of_int (((int_of_float ch) * 65536) + ((int_of_float ch) * 256) + (int_of_float ch))
  )

let generate_plasma_palette (size : int) : t =
  if size <= 0 then raise (Invalid_argument "Palette size must not be zero or negative");
  Array.init size (fun (index : int): int32 ->
    let fi = float_of_int index and fsize = float_of_int size in
    let fred = (cos (fi *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fgreen = (cos ((fi +. (fsize /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    let fblue = (cos ((fi +. ((fsize *. 2.0) /. 3.0)) *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in

    Int32.of_int (((int_of_float fred) * 65536) + ((int_of_float fgreen) * 256) + (int_of_float fblue))
  )

let string_to_chunks (x : string) (size : int) : string list =
  let rec loop sofar remainder =
    let length_left = String.length remainder in
    if length_left >= size then
      loop ((String.sub remainder 0 size) :: sofar) (String.sub remainder size (length_left - size))
    else if length_left == 0 then
      sofar
    else
      raise (Invalid_argument "String size not a multiple of 6 chars per colour")
  in
  List.rev (loop [] x)

let chunks_to_colors (raw : string list) : t =
  Array.map (fun (colorstr : string): int32 -> Int32.of_int (int_of_string ("0x" ^ colorstr))) (Array.of_list raw)

let load_tic80_palette (raw : string) : t =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  if List.length strchunks > 0 then
    chunks_to_colors strchunks
  else
    raise (Invalid_argument "Palette size must not be zero or negative")

let size (palette : t) : int =
    Array.length palette

let index_to_rgb (palette : t) (index : int) : int32 =
  let palsize = Array.length palette in
  let index = index mod palsize in
  palette.(if index >= 0 then index else index + palsize)

let to_list (palette : t) : int list =
    List.map Int32.to_int (Array.to_list palette)

let of_list (rgb_list : int list) : t =
  if List.length rgb_list > 0 then
    Array.of_list (List.map Int32.of_int rgb_list)
  else
    raise (Invalid_argument "Palette size must not be zero or negative")
