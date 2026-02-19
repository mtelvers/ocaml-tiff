(** Horizontal differencing predictor for TIFF.

    Predictor=2 means each sample is stored as the difference from
    the sample to its left. The first sample in each row is stored as-is.
    Undoing this requires accumulating left-to-right per row. *)

let undo_horizontal ~order ~width ~samples_per_pixel ~bits_per_sample data =
  if bits_per_sample <> 8 && bits_per_sample <> 16 && bits_per_sample <> 32 then
    data  (* Only apply to 8-bit, 16-bit, and 32-bit data *)
  else
    let bytes_per_sample = bits_per_sample / 8 in
    let row_bytes = width * samples_per_pixel * bytes_per_sample in
    let len = Bytes.length data in
    let result = Bytes.copy data in
    let num_rows = len / row_bytes in
    for row = 0 to num_rows - 1 do
      let row_off = row * row_bytes in
      if bytes_per_sample = 1 then begin
        (* 8-bit: accumulate per channel *)
        for i = samples_per_pixel to width * samples_per_pixel - 1 do
          let prev = Bytes.get_uint8 result (row_off + i - samples_per_pixel) in
          let cur = Bytes.get_uint8 result (row_off + i) in
          Bytes.set_uint8 result (row_off + i) ((prev + cur) land 0xFF)
        done
      end else if bytes_per_sample = 2 then begin
        (* 16-bit: accumulate per channel, 2 bytes each *)
        for i = samples_per_pixel to width * samples_per_pixel - 1 do
          let off = row_off + i * 2 in
          let prev_off = row_off + (i - samples_per_pixel) * 2 in
          let prev = Byte_order.get_uint16 order result prev_off in
          let cur = Byte_order.get_uint16 order result off in
          Byte_order.set_uint16 order result off ((prev + cur) land 0xFFFF)
        done
      end else begin
        (* 32-bit: accumulate per channel, 4 bytes each *)
        for i = samples_per_pixel to width * samples_per_pixel - 1 do
          let off = row_off + i * 4 in
          let prev_off = row_off + (i - samples_per_pixel) * 4 in
          let prev = Byte_order.get_int32 order result prev_off in
          let cur = Byte_order.get_int32 order result off in
          Byte_order.set_int32 order result off (Int32.add prev cur)
        done
      end
    done;
    result

let apply_horizontal ~order ~width ~samples_per_pixel ~bits_per_sample data =
  if bits_per_sample <> 8 && bits_per_sample <> 16 && bits_per_sample <> 32 then
    data
  else
    let bytes_per_sample = bits_per_sample / 8 in
    let row_bytes = width * samples_per_pixel * bytes_per_sample in
    let len = Bytes.length data in
    let result = Bytes.copy data in
    let num_rows = len / row_bytes in
    for row = 0 to num_rows - 1 do
      let row_off = row * row_bytes in
      if bytes_per_sample = 1 then begin
        (* Work right-to-left to avoid overwriting needed values *)
        for i = width * samples_per_pixel - 1 downto samples_per_pixel do
          let cur = Bytes.get_uint8 result (row_off + i) in
          let prev = Bytes.get_uint8 result (row_off + i - samples_per_pixel) in
          Bytes.set_uint8 result (row_off + i) ((cur - prev) land 0xFF)
        done
      end else if bytes_per_sample = 2 then begin
        for i = width * samples_per_pixel - 1 downto samples_per_pixel do
          let off = row_off + i * 2 in
          let prev_off = row_off + (i - samples_per_pixel) * 2 in
          let cur = Byte_order.get_uint16 order result off in
          let prev = Byte_order.get_uint16 order result prev_off in
          Byte_order.set_uint16 order result off ((cur - prev) land 0xFFFF)
        done
      end else begin
        for i = width * samples_per_pixel - 1 downto samples_per_pixel do
          let off = row_off + i * 4 in
          let prev_off = row_off + (i - samples_per_pixel) * 4 in
          let cur = Byte_order.get_int32 order result off in
          let prev = Byte_order.get_int32 order result prev_off in
          Byte_order.set_int32 order result off (Int32.sub cur prev)
        done
      end
    done;
    result
