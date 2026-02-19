(** Bit depth normalization for TIFF.

    Converts sub-byte (1, 2, 4-bit) and 16-bit samples to 8-bit.
    Sub-byte samples are expanded; 16-bit samples are scaled by dividing by 257. *)

let expand_1bit data width height =
  let out_len = width * height in
  let out = Bytes.create out_len in
  let src_row_bytes = (width + 7) / 8 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let byte_idx = y * src_row_bytes + x / 8 in
      let bit_idx = 7 - (x mod 8) in
      let bit = (Bytes.get_uint8 data byte_idx lsr bit_idx) land 1 in
      Bytes.set_uint8 out (y * width + x) (if bit = 1 then 255 else 0)
    done
  done;
  out

let expand_2bit data width height =
  let out_len = width * height in
  let out = Bytes.create out_len in
  let src_row_bytes = (width + 3) / 4 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let byte_idx = y * src_row_bytes + x / 4 in
      let shift = 6 - (x mod 4) * 2 in
      let val2 = (Bytes.get_uint8 data byte_idx lsr shift) land 3 in
      (* Scale 0-3 to 0-255 *)
      Bytes.set_uint8 out (y * width + x) (val2 * 85)
    done
  done;
  out

let expand_4bit data width height =
  let out_len = width * height in
  let out = Bytes.create out_len in
  let src_row_bytes = (width + 1) / 2 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let byte_idx = y * src_row_bytes + x / 2 in
      let val4 =
        if x mod 2 = 0 then (Bytes.get_uint8 data byte_idx lsr 4) land 0xF
        else Bytes.get_uint8 data byte_idx land 0xF
      in
      (* Scale 0-15 to 0-255 *)
      Bytes.set_uint8 out (y * width + x) (val4 * 17)
    done
  done;
  out

let scale_16_to_8 data =
  let len = Bytes.length data in
  let out_len = len / 2 in
  let out = Bytes.create out_len in
  for i = 0 to out_len - 1 do
    (* Read 16-bit value as little-endian - actual byte order already resolved *)
    let hi = Bytes.get_uint8 data (i * 2) in
    let lo = Bytes.get_uint8 data (i * 2 + 1) in
    let v = (hi lsl 8) lor lo in
    (* Scale by dividing by 257 (maps 0-65535 to 0-255) *)
    Bytes.set_uint8 out i (v / 257)
  done;
  out

let normalize ~bits_per_sample ~width ~height ~samples_per_pixel data =
  match bits_per_sample with
  | 8 -> data
  | 1 ->
    if samples_per_pixel = 1 then expand_1bit data width height
    else data
  | 2 ->
    if samples_per_pixel = 1 then expand_2bit data width height
    else data
  | 4 ->
    if samples_per_pixel = 1 then expand_4bit data width height
    else data
  | 16 -> scale_16_to_8 data
  | n -> failwith (Printf.sprintf "TIFF: unsupported bits per sample: %d" n)
