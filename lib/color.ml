(** Photometric interpretation conversion for TIFF.

    Converts various TIFF photometric interpretations to RGB/RGBA
    pixel data suitable for the output image format. *)

let invert_grayscale data =
  let len = Bytes.length data in
  let out = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set_uint8 out i (255 - Bytes.get_uint8 data i)
  done;
  out

let grayscale_to_rgb data =
  let len = Bytes.length data in
  let out = Bytes.create (len * 3) in
  for i = 0 to len - 1 do
    let v = Bytes.get_uint8 data i in
    let off = i * 3 in
    Bytes.set_uint8 out off v;
    Bytes.set_uint8 out (off + 1) v;
    Bytes.set_uint8 out (off + 2) v
  done;
  out

let grayscale_alpha_to_rgba data =
  let pixels = Bytes.length data / 2 in
  let out = Bytes.create (pixels * 4) in
  for i = 0 to pixels - 1 do
    let v = Bytes.get_uint8 data (i * 2) in
    let a = Bytes.get_uint8 data (i * 2 + 1) in
    let off = i * 4 in
    Bytes.set_uint8 out off v;
    Bytes.set_uint8 out (off + 1) v;
    Bytes.set_uint8 out (off + 2) v;
    Bytes.set_uint8 out (off + 3) a
  done;
  out

let palette_to_rgb order buf ifd ~ifd_offset ~width ~height data =
  let color_map_entry, color_map_idx =
    match Ifd.find_entry_index ifd Tags.color_map with
    | Some v -> v
    | None -> failwith "TIFF: palette image missing ColorMap tag"
  in
  let cmap = Ifd.get_short_values order buf color_map_entry
    ~ifd_offset ~entry_index:color_map_idx in
  let num_colors = Array.length cmap / 3 in
  let pixels = width * height in
  let out = Bytes.create (pixels * 3) in
  for i = 0 to pixels - 1 do
    let idx = Bytes.get_uint8 data i in
    let off = i * 3 in
    (* TIFF palette stores R values, then G values, then B values
       each as 16-bit values, scale to 8-bit *)
    Bytes.set_uint8 out off (cmap.(idx) / 256);
    Bytes.set_uint8 out (off + 1) (cmap.(num_colors + idx) / 256);
    Bytes.set_uint8 out (off + 2) (cmap.(2 * num_colors + idx) / 256)
  done;
  out

let clamp v = max 0 (min 255 v)

let ycbcr_to_rgb data =
  let pixels = Bytes.length data / 3 in
  let out = Bytes.create (pixels * 3) in
  for i = 0 to pixels - 1 do
    let off = i * 3 in
    let y = Bytes.get_uint8 data off in
    let cb = Bytes.get_uint8 data (off + 1) in
    let cr = Bytes.get_uint8 data (off + 2) in
    let yf = float_of_int y in
    let cbf = float_of_int cb -. 128.0 in
    let crf = float_of_int cr -. 128.0 in
    let r = clamp (int_of_float (yf +. 1.402 *. crf +. 0.5)) in
    let g = clamp (int_of_float (yf -. 0.344136 *. cbf -. 0.714136 *. crf +. 0.5)) in
    let b = clamp (int_of_float (yf +. 1.772 *. cbf +. 0.5)) in
    Bytes.set_uint8 out off r;
    Bytes.set_uint8 out (off + 1) g;
    Bytes.set_uint8 out (off + 2) b
  done;
  out

let cmyk_to_rgb data =
  let pixels = Bytes.length data / 4 in
  let out = Bytes.create (pixels * 3) in
  for i = 0 to pixels - 1 do
    let off_in = i * 4 in
    let c = Bytes.get_uint8 data off_in in
    let m = Bytes.get_uint8 data (off_in + 1) in
    let y = Bytes.get_uint8 data (off_in + 2) in
    let k = Bytes.get_uint8 data (off_in + 3) in
    let off_out = i * 3 in
    (* CMYK to RGB: R = (255-C)*(255-K)/255, etc. *)
    Bytes.set_uint8 out off_out ((255 - c) * (255 - k) / 255);
    Bytes.set_uint8 out (off_out + 1) ((255 - m) * (255 - k) / 255);
    Bytes.set_uint8 out (off_out + 2) ((255 - y) * (255 - k) / 255)
  done;
  out

type result = RGB of bytes | RGBA of bytes

let convert ~photometric ~samples_per_pixel ~extra_samples
    order buf ifd ~ifd_offset ~width ~height data =
  let has_alpha =
    samples_per_pixel > 3 && photometric = Tags.photometric_rgb ||
    samples_per_pixel > 1 && (photometric = Tags.photometric_white_is_zero ||
                              photometric = Tags.photometric_black_is_zero) ||
    (match extra_samples with
     | Some arr -> Array.length arr > 0 &&
                   (arr.(0) = Tags.extra_assoc_alpha ||
                    arr.(0) = Tags.extra_unassoc_alpha)
     | None -> false)
  in
  match photometric with
  | 0 (* WhiteIsZero *) ->
    let inverted = invert_grayscale data in
    if has_alpha && samples_per_pixel >= 2 then
      RGBA (grayscale_alpha_to_rgba inverted)
    else
      RGB (grayscale_to_rgb inverted)
  | 1 (* BlackIsZero *) ->
    if has_alpha && samples_per_pixel >= 2 then
      RGBA (grayscale_alpha_to_rgba data)
    else
      RGB (grayscale_to_rgb data)
  | 2 (* RGB *) ->
    if has_alpha && samples_per_pixel >= 4 then
      RGBA data
    else if samples_per_pixel >= 4 then
      (* Strip extra samples *)
      let pixels = width * height in
      let out = Bytes.create (pixels * 3) in
      for i = 0 to pixels - 1 do
        Bytes.blit data (i * samples_per_pixel) out (i * 3) 3
      done;
      RGB out
    else
      RGB data
  | 3 (* Palette *) ->
    RGB (palette_to_rgb order buf ifd ~ifd_offset ~width ~height data)
  | 5 (* CMYK *) ->
    RGB (cmyk_to_rgb data)
  | 6 (* YCbCr *) ->
    RGB (ycbcr_to_rgb data)
  | n ->
    failwith (Printf.sprintf "TIFF: unsupported photometric interpretation %d" n)
