(** Strip-based image data assembly for TIFF.

    Reads strip offsets and byte counts, decompresses each strip,
    optionally applies predictor, and concatenates into a single
    raster buffer. *)

let read_strips order buf ifd ~ifd_offset ~compression ~predictor
    ~width ~samples_per_pixel ~bits_per_sample =
  (* Get strip offsets *)
  let strip_offsets_entry, strip_offsets_idx =
    match Ifd.find_entry_index ifd Tags.strip_offsets with
    | Some v -> v
    | None -> failwith "TIFF: missing StripOffsets tag"
  in
  let strip_byte_counts_entry, strip_byte_counts_idx =
    match Ifd.find_entry_index ifd Tags.strip_byte_counts with
    | Some v -> v
    | None -> failwith "TIFF: missing StripByteCounts tag"
  in
  let offsets = Ifd.get_int_values order buf strip_offsets_entry
    ~ifd_offset ~entry_index:strip_offsets_idx in
  let byte_counts = Ifd.get_int_values order buf strip_byte_counts_entry
    ~ifd_offset ~entry_index:strip_byte_counts_idx in
  let num_strips = Array.length offsets in
  (* Read and decompress each strip *)
  let output = Buffer.create (width * samples_per_pixel * 1024) in
  for i = 0 to num_strips - 1 do
    let off = offsets.(i) in
    let count = byte_counts.(i) in
    let strip_data = Bytes.sub buf off count in
    let decompressed = Compression.decompress ~compression strip_data in
    (* Apply predictor if needed *)
    let final =
      if predictor = Tags.predictor_horizontal then
        Predict.undo_horizontal ~order ~width ~samples_per_pixel ~bits_per_sample decompressed
      else
        decompressed
    in
    Buffer.add_bytes output final
  done;
  Buffer.contents output |> Bytes.of_string
