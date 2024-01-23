open Claudius

let tick t s fb = 
  match (Screen.font s) with 
  | None -> fb
  | Some font -> (
    let prose = "Hello, world!" in
    let slow_t = t / 100 in
    let index = slow_t mod (String.length prose) in
    let c = String.get prose index in
    match Font.glyph_of_char font (Uchar.of_char c) with
    | None -> fb
    | Some glyph -> (
      let x = 10 and y = 10 in
      let gw, gh = Font.glyph_dimensions glyph in
      let bmp = Font.glyph_bitmap glyph in
      let bytes_per_line = (Bytes.length bmp) / gh in
      for h = 0 to (gh - 1) do
        for w = 0 to (bytes_per_line - 1) do
          let bitcount = if (((w + 1) * 8) < gw) then 8 else ((gw - (w * 8)) mod 8) in
          let b = int_of_char (Bytes.get bmp ((h * bytes_per_line) + w)) in
          for bit = 0 to (bitcount - 1) do
            let col = (b lsl bit) land 0x80 in
            (Framebuffer.pixel_write 
              (x + (w * 8) + bit)
              (y + h) 
              (if col > 0 then 15 else 8) 
              fb)
          done
        done
      done; fb
    )
  )

let () = 
  match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 16))) |>
    Screen.create_with_font 240 136 3 font |>
    Base.run "Font testing" None tick
  )
