type t = int32 array

exception String_not_multiple_of_chunk_size

let generate_mono_palette (size : int) : t = 
  Array.init size (fun (index : int): int32 ->
    let fi = float_of_int index and fsize = float_of_int size in
    let ch = ((fi /. fsize) *. 255.0) in
    Int32.of_int (((int_of_float ch) * 65536) + ((int_of_float ch) * 256) + (int_of_float ch))
  )

let generate_plasma_palette (size : int) : t = 
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
      raise String_not_multiple_of_chunk_size
  in
  List.rev (loop [] x)

let chunks_to_colors (raw : string list) : t =
  Array.map (fun (colorstr : string): int32 -> Int32.of_int (int_of_string ("0x" ^ colorstr))) (Array.of_list raw)

let load_tic80_palette (raw : string) : t =
  let parts = String.split_on_char ':' raw in
  let strchunks = string_to_chunks (List.nth parts 1) 6 in
  chunks_to_colors strchunks

let size (palette : t) : int =
    Array.length palette

let index_to_rgb (palette : t) (index : int) : int32 option =
  try
    Some palette.(index)
  with
  | Invalid_argument(_) -> None


let to_list (palette : t) : int list =
    List.map Int32.to_int (Array.to_list palette)

let of_list (rgb_list : int list) : t = 
    Array.of_list (List.map Int32.of_int rgb_list)
  