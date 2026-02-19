(** Basic usage example for the TIFF library. *)

let () =
  (* Create a simple 100x100 RGB gradient image *)
  let w = 100 and h = 100 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let off = (y * w + x) * 3 in
      Bigarray.Array1.set pixels off (x * 255 / w);        (* R *)
      Bigarray.Array1.set pixels (off + 1) (y * 255 / h);  (* G *)
      Bigarray.Array1.set pixels (off + 2) 128              (* B *)
    done
  done;
  let img = Tiff.create_image w h pixels in
  (* Write with different compression methods *)
  Printf.printf "Writing gradient_none.tiff (no compression)...\n";
  Tiff.write_with_options
    { Tiff.compression = Compress_none; tile_size = None; predictor = false }
    "gradient_none.tiff" img;
  Printf.printf "Writing gradient_lzw.tiff (LZW compression)...\n";
  Tiff.write "gradient_lzw.tiff" img;
  Printf.printf "Writing gradient_packbits.tiff (PackBits compression)...\n";
  Tiff.write_with_options
    { Tiff.compression = Compress_packbits; tile_size = None; predictor = false }
    "gradient_packbits.tiff" img;
  Printf.printf "Writing gradient_tiled.tiff (tiled, LZW)...\n";
  Tiff.write_with_options
    { Tiff.compression = Compress_lzw; tile_size = Some 32; predictor = false }
    "gradient_tiled.tiff" img;
  (* Read back and verify *)
  Printf.printf "Reading back gradient_lzw.tiff...\n";
  let img2 = Tiff.read "gradient_lzw.tiff" in
  Printf.printf "  Size: %dx%d\n" img2.width img2.height;
  let r, g, b = Tiff.get_pixel img2 50 50 in
  Printf.printf "  Pixel at (50,50): R=%d G=%d B=%d\n" r g b;
  Printf.printf "Done!\n"
