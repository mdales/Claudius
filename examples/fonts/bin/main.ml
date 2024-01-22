open Claudius

let glyphs : ((bytes array) option) ref = ref None

let tick t _s fb = 
  match !glyphs with 
  | None -> fb
  | Some glyphs -> (
    let slow_t = t / 100 in
    let index = slow_t mod (Array.length glyphs) in
    let glyph = glyphs.(index) in
    let x = 10 and y = 10 in
    let bytes_per_line = (Bytes.length glyph) / 20 in
    for h = 0 to 19 do
      for w = 0 to (bytes_per_line - 1) do
        (* let bitcount = if (((w + 1) * 8) < 10) then 8 else ((10 - (w * 8)) mod 8) in *)
        let b = int_of_char (Bytes.get glyph ((h * bytes_per_line) + w)) in
        for bit = 0 to 7 do
          let col = (b lsr bit) land 0x1 in
          Framebuffer.pixel_write (x + (8 * bytes_per_line) - (((bytes_per_line - 1 - w) * 8) + bit)) (y + h) (if col == 1 then 15 else 8) fb;
        done
      done
    done; fb
  )

  (* https://wiki.osdev.org/PC_Screen_Font *)

type psfheader = {
  magic : int32 ;
  version : int32 ;
  headersize : int32 ;
  flags : int32 ;
  number_of_glyphs : int32 ;
  bytes_per_glyph : int32 ;
  height : int32 ;
  width : int32 ;
}

let read_header (ic : in_channel) : (psfheader, string) result =
  let header_buffer = Bytes.create (4 * 8) in
  try 
    really_input ic header_buffer 0 (Bytes.length header_buffer);
    Result.ok  {
      magic = Bytes.get_int32_le header_buffer 0 ;
      version = Bytes.get_int32_le header_buffer 4 ;
      headersize = Bytes.get_int32_le header_buffer 8 ;
      flags = Bytes.get_int32_le header_buffer 12 ;
      number_of_glyphs = Bytes.get_int32_le header_buffer 16 ;
      bytes_per_glyph = Bytes.get_int32_le header_buffer 20 ;
      height = Bytes.get_int32_le header_buffer 24 ;
      width = Bytes.get_int32_le header_buffer 28 ;
    }
  with 
  | Sys_error(reason) -> Result.error reason

let display_header header = 
  Printf.printf "Magic:            0x%08x\n" ((Int32.to_int header.magic) land 0xFFFFFFFF);
  Printf.printf "Version:          %d\n" (Int32.to_int header.version);
  Printf.printf "Header Size:      %d\n" (Int32.to_int header.headersize);
  Printf.printf "Flags:            0x%08x\n" ((Int32.to_int header.flags) land 0xFFFFFFFF);
  Printf.printf "Number of Glyphs: %d\n" (Int32.to_int header.number_of_glyphs);
  Printf.printf "Bytes per Glyph:  %d\n" (Int32.to_int header.bytes_per_glyph);
  Printf.printf "Width:            %d\n" (Int32.to_int header.width);
  Printf.printf "Height:           %d\n" (Int32.to_int header.height)

let load_glyphs ic header : (bytes array, string) result =
  let bpg = Int32.to_int header.bytes_per_glyph in
  try
    Result.ok (Array.init (Int32.to_int header.number_of_glyphs) (fun i -> 
      let buffer = Bytes.create bpg in
      In_channel.seek ic (Int64.of_int ((Int32.to_int header.headersize) + (i * bpg)));
      really_input ic buffer 0 bpg;
      buffer
    ))
  with 
  | Sys_error(reason) -> Result.error reason

let () = 
  (
    let filename = "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" in
    let res = In_channel.with_open_bin filename (fun ic ->
      match read_header ic with 
      | Ok (header) -> (
        match load_glyphs ic header with
        | Ok glyphs -> Result.ok (header, glyphs)
        | Error (reason) -> Result.error reason
      )
      | Error (reason) -> Result.error reason
    ) in
    match res with
    | Ok (header, fglyphs) -> (display_header header; glyphs := Some fglyphs)
    | Error (reason) -> Printf.printf "Failed to read: %s" reason
  );

  Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 16))) |>
  Screen.create 240 136 3 |>
  Base.run "Font testing" None tick
