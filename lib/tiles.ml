(** Tile-based image data assembly for TIFF.

    Reads tile offsets and byte counts, decompresses each tile,
    clips edge tiles, and assembles into the full raster. *)

let read_tiles order buf ifd ~ifd_offset ~compression ~predictor
    ~width ~height ~tile_width ~tile_height
    ~samples_per_pixel ~bits_per_sample =
  let tile_offsets_entry, tile_offsets_idx =
    match Ifd.find_entry_index ifd Tags.tile_offsets with
    | Some v -> v
    | None -> failwith "TIFF: missing TileOffsets tag"
  in
  let tile_byte_counts_entry, tile_byte_counts_idx =
    match Ifd.find_entry_index ifd Tags.tile_byte_counts with
    | Some v -> v
    | None -> failwith "TIFF: missing TileByteCounts tag"
  in
  let offsets = Ifd.get_int_values order buf tile_offsets_entry
    ~ifd_offset ~entry_index:tile_offsets_idx in
  let byte_counts = Ifd.get_int_values order buf tile_byte_counts_entry
    ~ifd_offset ~entry_index:tile_byte_counts_idx in
  let bytes_per_sample = max 1 (bits_per_sample / 8) in
  let pixel_bytes = samples_per_pixel * bytes_per_sample in
  let row_bytes = width * pixel_bytes in
  let output = Bytes.create (height * row_bytes) in
  let tiles_across = (width + tile_width - 1) / tile_width in
  let tiles_down = (height + tile_height - 1) / tile_height in
  for ty = 0 to tiles_down - 1 do
    for tx = 0 to tiles_across - 1 do
      let tile_idx = ty * tiles_across + tx in
      let off = offsets.(tile_idx) in
      let count = byte_counts.(tile_idx) in
      let tile_data = Bytes.sub buf off count in
      let decompressed = Compression.decompress ~compression tile_data in
      let final =
        if predictor = Tags.predictor_horizontal then
          Predict.undo_horizontal ~order ~width:tile_width ~samples_per_pixel
            ~bits_per_sample decompressed
        else
          decompressed
      in
      (* Copy tile into output, clipping at image edges *)
      let x0 = tx * tile_width in
      let y0 = ty * tile_height in
      let copy_w = min tile_width (width - x0) in
      let copy_h = min tile_height (height - y0) in
      let tile_row_bytes = tile_width * pixel_bytes in
      for row = 0 to copy_h - 1 do
        let src_off = row * tile_row_bytes in
        let dst_off = (y0 + row) * row_bytes + x0 * pixel_bytes in
        Bytes.blit final src_off output dst_off (copy_w * pixel_bytes)
      done
    done
  done;
  output
