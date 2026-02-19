(** Test suite for the TIFF library. *)

let test_byte_order_le () =
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0x49;
  Bytes.set_uint8 buf 1 0x49;
  let order = Tiff.Byte_order.detect buf in
  Alcotest.(check bool) "little-endian" true (order = Tiff.Byte_order.LE)

let test_byte_order_be () =
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0x4D;
  Bytes.set_uint8 buf 1 0x4D;
  let order = Tiff.Byte_order.detect buf in
  Alcotest.(check bool) "big-endian" true (order = Tiff.Byte_order.BE)

let test_byte_order_invalid () =
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0x00;
  Bytes.set_uint8 buf 1 0x00;
  Alcotest.check_raises "invalid byte order" (Failure "TIFF: invalid byte order marker")
    (fun () -> ignore (Tiff.Byte_order.detect buf))

let test_uint16_le () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x2A;
  Bytes.set_uint8 buf 1 0x00;
  let v = Tiff.Byte_order.get_uint16 Tiff.Byte_order.LE buf 0 in
  Alcotest.(check int) "uint16 LE" 42 v

let test_uint16_be () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x00;
  Bytes.set_uint8 buf 1 0x2A;
  let v = Tiff.Byte_order.get_uint16 Tiff.Byte_order.BE buf 0 in
  Alcotest.(check int) "uint16 BE" 42 v

let test_packbits_decompress () =
  (* Test literal run: header=1 means copy 2 bytes *)
  let data = Bytes.of_string "\x01\xAB\xCD" in
  let result = Tiff.Packbits.decompress data in
  Alcotest.(check int) "length" 2 (Bytes.length result);
  Alcotest.(check int) "byte 0" 0xAB (Bytes.get_uint8 result 0);
  Alcotest.(check int) "byte 1" 0xCD (Bytes.get_uint8 result 1)

let test_packbits_repeat () =
  (* Test repeat run: header=0xFD (-3) means repeat 4 times *)
  let data = Bytes.create 2 in
  Bytes.set_uint8 data 0 0xFD;
  Bytes.set_uint8 data 1 0x42;
  let result = Tiff.Packbits.decompress data in
  Alcotest.(check int) "length" 4 (Bytes.length result);
  for i = 0 to 3 do
    Alcotest.(check int) (Printf.sprintf "byte %d" i) 0x42 (Bytes.get_uint8 result i)
  done

let test_packbits_roundtrip () =
  let original = Bytes.of_string "Hello, TIFF world! This is a test of PackBits compression." in
  let compressed = Tiff.Packbits.compress original in
  let decompressed = Tiff.Packbits.decompress compressed in
  Alcotest.(check bytes) "roundtrip" original decompressed

let test_lzw_roundtrip () =
  (* Small data: stays within 9-bit codes *)
  let original = Bytes.make 256 '\x00' in
  for i = 0 to 255 do
    Bytes.set_uint8 original i (i mod 128)
  done;
  let compressed = Tiff.Lzw.compress original in
  let decompressed = Tiff.Lzw.decompress compressed in
  Alcotest.(check int) "length" (Bytes.length original) (Bytes.length decompressed);
  Alcotest.(check bytes) "roundtrip" original decompressed;
  (* Large data: exercises 9->10->11->12 bit code-size transitions *)
  let large = Bytes.create 100_000 in
  for i = 0 to 99_999 do
    Bytes.set_uint8 large i ((i * 7 + i / 256) mod 256)
  done;
  let lc = Tiff.Lzw.compress large in
  let ld = Tiff.Lzw.decompress lc in
  Alcotest.(check int) "large length" (Bytes.length large) (Bytes.length ld);
  Alcotest.(check bytes) "large roundtrip" large ld

let test_deflate_roundtrip () =
  let original = Bytes.of_string "Test deflate data for TIFF compression roundtrip." in
  let compressed = Tiff.Deflate.deflate original in
  let decompressed = Tiff.Deflate.inflate compressed in
  Alcotest.(check bytes) "roundtrip" original decompressed

let test_predict_horizontal () =
  (* 2 pixels wide, 1 channel, 8-bit *)
  let data = Bytes.create 2 in
  Bytes.set_uint8 data 0 100;
  Bytes.set_uint8 data 1 50;  (* delta *)
  let result = Tiff.Predict.undo_horizontal ~order:Tiff.Byte_order.LE ~width:2
    ~samples_per_pixel:1 ~bits_per_sample:8 data in
  Alcotest.(check int) "pixel 0" 100 (Bytes.get_uint8 result 0);
  Alcotest.(check int) "pixel 1" 150 (Bytes.get_uint8 result 1)

let test_predict_roundtrip () =
  let data = Bytes.create 6 in
  Bytes.set_uint8 data 0 100;
  Bytes.set_uint8 data 1 150;
  Bytes.set_uint8 data 2 200;
  Bytes.set_uint8 data 3 110;
  Bytes.set_uint8 data 4 160;
  Bytes.set_uint8 data 5 210;
  let encoded = Tiff.Predict.apply_horizontal ~order:Tiff.Byte_order.LE ~width:2
    ~samples_per_pixel:3 ~bits_per_sample:8 data in
  let decoded = Tiff.Predict.undo_horizontal ~order:Tiff.Byte_order.LE ~width:2
    ~samples_per_pixel:3 ~bits_per_sample:8 encoded in
  Alcotest.(check bytes) "roundtrip" data decoded

let test_sample_1bit () =
  (* 8 pixels wide, 1 row, 1 byte *)
  let data = Bytes.create 1 in
  Bytes.set_uint8 data 0 0b10101010;
  let result = Tiff.Sample.expand_1bit data 8 1 in
  Alcotest.(check int) "pixel 0" 255 (Bytes.get_uint8 result 0);
  Alcotest.(check int) "pixel 1" 0 (Bytes.get_uint8 result 1);
  Alcotest.(check int) "pixel 2" 255 (Bytes.get_uint8 result 2);
  Alcotest.(check int) "pixel 3" 0 (Bytes.get_uint8 result 3)

let test_sample_4bit () =
  let data = Bytes.create 1 in
  Bytes.set_uint8 data 0 0xF0;  (* high nibble = 15, low nibble = 0 *)
  let result = Tiff.Sample.expand_4bit data 2 1 in
  Alcotest.(check int) "pixel 0" 255 (Bytes.get_uint8 result 0);
  Alcotest.(check int) "pixel 1" 0 (Bytes.get_uint8 result 1)

let test_sample_16_to_8 () =
  let data = Bytes.create 4 in
  (* 65535 (0xFFFF) -> 255, 0 -> 0 *)
  Bytes.set_uint8 data 0 0xFF;
  Bytes.set_uint8 data 1 0xFF;
  Bytes.set_uint8 data 2 0x00;
  Bytes.set_uint8 data 3 0x00;
  let result = Tiff.Sample.scale_16_to_8 data in
  Alcotest.(check int) "max" 255 (Bytes.get_uint8 result 0);
  Alcotest.(check int) "min" 0 (Bytes.get_uint8 result 1)

let test_create_image () =
  let w = 4 and h = 3 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  Bigarray.Array1.fill pixels 128;
  let img = Tiff.create_image w h pixels in
  Alcotest.(check int) "width" w img.width;
  Alcotest.(check int) "height" h img.height;
  Alcotest.(check bool) "format" true (img.pixel_format = Tiff.RGB24)

let test_create_image_bad_size () =
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout 10 in
  Alcotest.check_raises "bad size"
    (Failure "TIFF: pixel data size mismatch (expected 12, got 10)")
    (fun () -> ignore (Tiff.create_image 2 2 pixels))

let test_get_set_pixel () =
  let w = 3 and h = 2 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  Bigarray.Array1.fill pixels 0;
  let img = Tiff.create_image w h pixels in
  Tiff.set_pixel img 1 0 255 128 64;
  let r, g, b = Tiff.get_pixel img 1 0 in
  Alcotest.(check int) "r" 255 r;
  Alcotest.(check int) "g" 128 g;
  Alcotest.(check int) "b" 64 b

let test_get_set_pixel_rgba () =
  let w = 2 and h = 2 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 4) in
  Bigarray.Array1.fill pixels 0;
  let img = Tiff.create_rgba_image w h pixels in
  Tiff.set_pixel_rgba img 0 1 10 20 30 128;
  let r, g, b, a = Tiff.get_pixel_rgba img 0 1 in
  Alcotest.(check int) "r" 10 r;
  Alcotest.(check int) "g" 20 g;
  Alcotest.(check int) "b" 30 b;
  Alcotest.(check int) "a" 128 a

let test_pixel_bounds () =
  let w = 2 and h = 2 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  let img = Tiff.create_image w h pixels in
  Alcotest.check_raises "out of bounds"
    (Failure "TIFF: pixel coordinates out of bounds")
    (fun () -> ignore (Tiff.get_pixel img 5 0))

let test_roundtrip_rgb_no_compression () =
  let w = 10 and h = 8 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for i = 0 to w * h * 3 - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let img = Tiff.create_image w h pixels in
  let opts = { Tiff.compression = Compress_none; tile_size = None; predictor = false } in
  let encoded = Tiff.write_bytes_with_options opts img in
  let decoded = Tiff.read_bytes encoded in
  Alcotest.(check int) "width" w decoded.width;
  Alcotest.(check int) "height" h decoded.height;
  for i = 0 to w * h * 3 - 1 do
    Alcotest.(check int) (Printf.sprintf "pixel %d" i)
      (Bigarray.Array1.get pixels i)
      (Bigarray.Array1.get decoded.pixels i)
  done

let test_roundtrip_rgb_lzw () =
  (* Use 100x80 image (24000 bytes) to exercise LZW code-size transitions *)
  let w = 100 and h = 80 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for i = 0 to w * h * 3 - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let img = Tiff.create_image w h pixels in
  let opts = { Tiff.compression = Compress_lzw; tile_size = None; predictor = false } in
  let encoded = Tiff.write_bytes_with_options opts img in
  let decoded = Tiff.read_bytes encoded in
  Alcotest.(check int) "width" w decoded.width;
  Alcotest.(check int) "height" h decoded.height;
  for i = 0 to w * h * 3 - 1 do
    Alcotest.(check int) (Printf.sprintf "pixel %d" i)
      (Bigarray.Array1.get pixels i)
      (Bigarray.Array1.get decoded.pixels i)
  done

let test_roundtrip_rgb_packbits () =
  let w = 10 and h = 8 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for i = 0 to w * h * 3 - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let img = Tiff.create_image w h pixels in
  let opts = { Tiff.compression = Compress_packbits; tile_size = None; predictor = false } in
  let encoded = Tiff.write_bytes_with_options opts img in
  let decoded = Tiff.read_bytes encoded in
  Alcotest.(check int) "width" w decoded.width;
  Alcotest.(check int) "height" h decoded.height;
  for i = 0 to w * h * 3 - 1 do
    Alcotest.(check int) (Printf.sprintf "pixel %d" i)
      (Bigarray.Array1.get pixels i)
      (Bigarray.Array1.get decoded.pixels i)
  done

let test_roundtrip_rgb_deflate () =
  let w = 10 and h = 8 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for i = 0 to w * h * 3 - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let img = Tiff.create_image w h pixels in
  let opts = { Tiff.compression = Compress_deflate; tile_size = None; predictor = false } in
  let encoded = Tiff.write_bytes_with_options opts img in
  let decoded = Tiff.read_bytes encoded in
  Alcotest.(check int) "width" w decoded.width;
  Alcotest.(check int) "height" h decoded.height;
  for i = 0 to w * h * 3 - 1 do
    Alcotest.(check int) (Printf.sprintf "pixel %d" i)
      (Bigarray.Array1.get pixels i)
      (Bigarray.Array1.get decoded.pixels i)
  done

let test_roundtrip_rgba () =
  let w = 4 and h = 4 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 4) in
  for i = 0 to w * h * 4 - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let img = Tiff.create_rgba_image w h pixels in
  let opts = { Tiff.compression = Compress_none; tile_size = None; predictor = false } in
  let encoded = Tiff.write_bytes_with_options opts img in
  let decoded = Tiff.read_bytes encoded in
  Alcotest.(check int) "width" w decoded.width;
  Alcotest.(check int) "height" h decoded.height;
  Alcotest.(check bool) "format" true (decoded.pixel_format = Tiff.RGBA32);
  for i = 0 to w * h * 4 - 1 do
    Alcotest.(check int) (Printf.sprintf "pixel %d" i)
      (Bigarray.Array1.get pixels i)
      (Bigarray.Array1.get decoded.pixels i)
  done

let test_roundtrip_tiled () =
  let w = 10 and h = 8 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for i = 0 to w * h * 3 - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let img = Tiff.create_image w h pixels in
  let opts = { Tiff.compression = Compress_none; tile_size = Some 4; predictor = false } in
  let encoded = Tiff.write_bytes_with_options opts img in
  let decoded = Tiff.read_bytes encoded in
  Alcotest.(check int) "width" w decoded.width;
  Alcotest.(check int) "height" h decoded.height;
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let r1, g1, b1 = Tiff.get_pixel img x y in
      let r2, g2, b2 = Tiff.get_pixel decoded x y in
      Alcotest.(check int) (Printf.sprintf "r@%d,%d" x y) r1 r2;
      Alcotest.(check int) (Printf.sprintf "g@%d,%d" x y) g1 g2;
      Alcotest.(check int) (Printf.sprintf "b@%d,%d" x y) b1 b2
    done
  done

let test_geokeys_parse () =
  (* Minimal GeoKey directory: version 1.1, 1 key *)
  let shorts = [| 1; 1; 0; 1;  (* header: v1, rev1.0, 1 key *)
                  3072; 0; 1; 32632  (* ProjectedCSTypeGeoKey = 32632 (UTM 32N) *)
               |] in
  match Tiff.Geokeys.parse_directory shorts with
  | None -> Alcotest.fail "failed to parse"
  | Some gk ->
    Alcotest.(check int) "version" 1 gk.version;
    Alcotest.(check (option int)) "epsg" (Some 32632)
      (Tiff.Geokeys.projected_cs_type gk)

let test_geotransform_tiepoint_scale () =
  let t = {
    Tiff.Geotransform.tiepoints = [{
      raster_x = 0.0; raster_y = 0.0; raster_z = 0.0;
      model_x = 500000.0; model_y = 4500000.0; model_z = 0.0;
    }];
    pixel_scale = Some {
      scale_x = 10.0; scale_y = 10.0; scale_z = 0.0;
    };
    transformation = None;
  } in
  match Tiff.Geotransform.raster_to_model t 100.0 200.0 with
  | None -> Alcotest.fail "no result"
  | Some (mx, my) ->
    Alcotest.(check (float 0.001)) "model x" 501000.0 mx;
    Alcotest.(check (float 0.001)) "model y" 4498000.0 my

let test_geotransform_roundtrip () =
  let t = {
    Tiff.Geotransform.tiepoints = [{
      raster_x = 0.0; raster_y = 0.0; raster_z = 0.0;
      model_x = 500000.0; model_y = 4500000.0; model_z = 0.0;
    }];
    pixel_scale = Some {
      scale_x = 10.0; scale_y = 10.0; scale_z = 0.0;
    };
    transformation = None;
  } in
  let rx, ry = 150.0, 250.0 in
  match Tiff.Geotransform.raster_to_model t rx ry with
  | None -> Alcotest.fail "no model result"
  | Some (mx, my) ->
    match Tiff.Geotransform.model_to_raster t mx my with
    | None -> Alcotest.fail "no raster result"
    | Some (rx2, ry2) ->
      Alcotest.(check (float 0.001)) "rx roundtrip" rx rx2;
      Alcotest.(check (float 0.001)) "ry roundtrip" ry ry2

let test_color_grayscale () =
  let data = Bytes.create 4 in
  Bytes.set_uint8 data 0 0;
  Bytes.set_uint8 data 1 128;
  Bytes.set_uint8 data 2 255;
  Bytes.set_uint8 data 3 64;
  let rgb = Tiff.Color.grayscale_to_rgb data in
  Alcotest.(check int) "r0" 0 (Bytes.get_uint8 rgb 0);
  Alcotest.(check int) "g0" 0 (Bytes.get_uint8 rgb 1);
  Alcotest.(check int) "b0" 0 (Bytes.get_uint8 rgb 2);
  Alcotest.(check int) "r1" 128 (Bytes.get_uint8 rgb 3)

let test_icc_tiff () =
  let data = Bytes.of_string "fake ICC profile data" in
  let icc = Tiff.Icc_tiff.from_bytes data in
  Alcotest.(check bool) "not empty" false (Tiff.Icc_tiff.is_empty icc);
  Alcotest.(check bytes) "roundtrip" data (Tiff.Icc_tiff.to_bytes icc)

let test_raster_uint8_roundtrip () =
  (* Write a normal RGB image, then read it back as raster *)
  let w = 4 and h = 3 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for i = 0 to w * h * 3 - 1 do
    Bigarray.Array1.set pixels i (i mod 256)
  done;
  let img = Tiff.create_image w h pixels in
  let opts = { Tiff.compression = Compress_none; tile_size = None; predictor = false } in
  let encoded = Tiff.write_bytes_with_options opts img in
  let r = Tiff.read_raster_bytes encoded in
  Alcotest.(check int) "width" w r.raster_width;
  Alcotest.(check int) "height" h r.raster_height;
  Alcotest.(check int) "bands" 3 r.bands;
  Alcotest.(check bool) "format" true (r.sample_format = Tiff.Uint8);
  match r.data with
  | Tiff.Raster_uint8 arr ->
    for i = 0 to w * h * 3 - 1 do
      Alcotest.(check int) (Printf.sprintf "sample %d" i)
        (Bigarray.Array1.get pixels i)
        (Bigarray.Array1.get arr i)
    done
  | _ -> Alcotest.fail "expected Raster_uint8"

(* Helper to build a minimal TIFF in memory with custom BitsPerSample and SampleFormat *)
let build_minimal_tiff ~width ~height ~samples_per_pixel ~bits_per_sample
    ~sample_format_val pixel_bytes =
  let order = Tiff.Byte_order.LE in
  let bps_array = Array.make samples_per_pixel bits_per_sample in
  let sf_array = Array.make samples_per_pixel sample_format_val in
  let bytes_per_sample = bits_per_sample / 8 in
  let row_bytes = width * samples_per_pixel * bytes_per_sample in
  let data_size = height * row_bytes in
  (* Build IFD entries - we need: ImageWidth, ImageLength, BitsPerSample,
     Compression, PhotometricInterpretation, SamplesPerPixel,
     RowsPerStrip, StripOffsets, StripByteCounts, SampleFormat *)
  let num_entries = 10 in
  let ifd_offset = 8 in
  let ifd_size = 2 + num_entries * 12 + 4 in
  (* External data: BitsPerSample (shorts), SampleFormat (shorts) *)
  let bps_ext_size = if samples_per_pixel * 2 > 4 then samples_per_pixel * 2 else 0 in
  let sf_ext_size = if samples_per_pixel * 2 > 4 then samples_per_pixel * 2 else 0 in
  let ext_size = bps_ext_size + sf_ext_size in
  let data_start = ifd_offset + ifd_size + ext_size in
  let total_size = data_start + data_size in
  let buf = Bytes.make total_size '\000' in
  (* Header *)
  Bytes.set_uint8 buf 0 0x49;
  Bytes.set_uint8 buf 1 0x49;
  Tiff.Byte_order.set_uint16 order buf 2 42;
  Tiff.Byte_order.set_uint32 order buf 4 ifd_offset;
  (* IFD entry count *)
  Tiff.Byte_order.set_uint16 order buf ifd_offset num_entries;
  let ext_pos = ref (ifd_offset + ifd_size) in
  let write_entry idx tag typ count value =
    let off = ifd_offset + 2 + idx * 12 in
    Tiff.Byte_order.set_uint16 order buf off tag;
    Tiff.Byte_order.set_uint16 order buf (off + 2) typ;
    Tiff.Byte_order.set_uint32 order buf (off + 4) count;
    Tiff.Byte_order.set_uint32 order buf (off + 8) value
  in
  let write_entry_shorts idx tag arr =
    let off = ifd_offset + 2 + idx * 12 in
    let count = Array.length arr in
    Tiff.Byte_order.set_uint16 order buf off tag;
    Tiff.Byte_order.set_uint16 order buf (off + 2) Tiff.Tags.type_short;
    Tiff.Byte_order.set_uint32 order buf (off + 4) count;
    if count * 2 <= 4 then begin
      for j = 0 to count - 1 do
        Tiff.Byte_order.set_uint16 order buf (off + 8 + j * 2) arr.(j)
      done
    end else begin
      Tiff.Byte_order.set_uint32 order buf (off + 8) !ext_pos;
      for j = 0 to count - 1 do
        Tiff.Byte_order.set_uint16 order buf (!ext_pos + j * 2) arr.(j)
      done;
      ext_pos := !ext_pos + count * 2
    end
  in
  (* Entries must be sorted by tag *)
  (* 256=ImageWidth, 257=ImageLength, 258=BitsPerSample, 259=Compression,
     262=PhotometricInterpretation, 277=SamplesPerPixel, 278=RowsPerStrip,
     273=StripOffsets, 279=StripByteCounts, 339=SampleFormat *)
  write_entry 0 Tiff.Tags.image_width Tiff.Tags.type_long 1 width;
  write_entry 1 Tiff.Tags.image_length Tiff.Tags.type_long 1 height;
  write_entry_shorts 2 Tiff.Tags.bits_per_sample bps_array;
  write_entry 3 Tiff.Tags.compression Tiff.Tags.type_short 1 Tiff.Tags.compression_none;
  write_entry 4 Tiff.Tags.photometric_interpretation Tiff.Tags.type_short 1
    (if samples_per_pixel >= 3 then Tiff.Tags.photometric_rgb
     else Tiff.Tags.photometric_black_is_zero);
  write_entry 5 Tiff.Tags.strip_offsets Tiff.Tags.type_long 1 data_start;
  write_entry 6 Tiff.Tags.samples_per_pixel Tiff.Tags.type_short 1 samples_per_pixel;
  write_entry 7 Tiff.Tags.rows_per_strip Tiff.Tags.type_long 1 height;
  write_entry 8 Tiff.Tags.strip_byte_counts Tiff.Tags.type_long 1 data_size;
  write_entry_shorts 9 Tiff.Tags.sample_format sf_array;
  (* Next IFD = 0 *)
  let next_off = ifd_offset + 2 + num_entries * 12 in
  Tiff.Byte_order.set_uint32 order buf next_off 0;
  (* Copy pixel data *)
  Bytes.blit pixel_bytes 0 buf data_start (min data_size (Bytes.length pixel_bytes));
  buf

let test_raster_uint16 () =
  let w = 2 and h = 2 in
  let spp = 1 in
  let order = Tiff.Byte_order.LE in
  (* Build raw pixel data: 4 uint16 values *)
  let pixel_bytes = Bytes.create (w * h * spp * 2) in
  let values = [| 0; 1000; 50000; 65535 |] in
  Array.iteri (fun i v ->
    Tiff.Byte_order.set_uint16 order pixel_bytes (i * 2) v
  ) values;
  let buf = build_minimal_tiff ~width:w ~height:h ~samples_per_pixel:spp
    ~bits_per_sample:16 ~sample_format_val:Tiff.Tags.sample_format_uint pixel_bytes in
  let r = Tiff.read_raster_bytes buf in
  Alcotest.(check int) "width" w r.raster_width;
  Alcotest.(check int) "height" h r.raster_height;
  Alcotest.(check bool) "format" true (r.sample_format = Tiff.Uint16);
  match r.data with
  | Tiff.Raster_uint16 arr ->
    Array.iteri (fun i v ->
      Alcotest.(check int) (Printf.sprintf "sample %d" i) v
        (Bigarray.Array1.get arr i)
    ) values
  | _ -> Alcotest.fail "expected Raster_uint16"

let test_raster_float32 () =
  let w = 2 and h = 2 in
  let spp = 1 in
  let order = Tiff.Byte_order.LE in
  (* Build raw pixel data: 4 float32 values *)
  let pixel_bytes = Bytes.create (w * h * spp * 4) in
  let values = [| 0.0; 1523.7; -9999.0; 3.14159 |] in
  Array.iteri (fun i v ->
    Tiff.Byte_order.set_int32 order pixel_bytes (i * 4) (Int32.bits_of_float v)
  ) values;
  let buf = build_minimal_tiff ~width:w ~height:h ~samples_per_pixel:spp
    ~bits_per_sample:32 ~sample_format_val:Tiff.Tags.sample_format_float pixel_bytes in
  let r = Tiff.read_raster_bytes buf in
  Alcotest.(check int) "width" w r.raster_width;
  Alcotest.(check int) "height" h r.raster_height;
  Alcotest.(check bool) "format" true (r.sample_format = Tiff.Float32);
  match r.data with
  | Tiff.Raster_float32 arr ->
    Array.iteri (fun i v ->
      Alcotest.(check (float 0.01)) (Printf.sprintf "sample %d" i) v
        (Bigarray.Array1.get arr i)
    ) values
  | _ -> Alcotest.fail "expected Raster_float32"

(* ---- Python interop tests ---- *)

let python_available = lazy (
  Sys.command "python3 -c 'import PIL, tifffile' 2>/dev/null" = 0
)

let tmpdir = lazy (
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "ocaml_tiff_test" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir
)

let run_python script =
  let dir = Lazy.force tmpdir in
  let script_file = Filename.concat dir "test_script.py" in
  let log_file = Filename.concat dir "test_output.log" in
  let oc = open_out script_file in
  output_string oc script;
  close_out oc;
  let cmd = Printf.sprintf "python3 %s > %s 2>&1"
    (Filename.quote script_file) (Filename.quote log_file) in
  let rc = Sys.command cmd in
  if rc <> 0 then begin
    let ic = open_in log_file in
    let log = In_channel.input_all ic in
    close_in ic;
    Alcotest.failf "Python script failed (rc=%d):\n%s" rc log
  end

let skip_without_python f () =
  if Lazy.force python_available then f ()
  else Alcotest.(check pass) "SKIP: python3 with PIL/tifffile not available" () ()

let write_test_image dir name opts =
  let w = 100 and h = 80 in
  let bpp = match opts.Tiff.compression with _ -> 3 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * bpp) in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let off = (y * w + x) * bpp in
      Bigarray.Array1.set pixels off (x * 255 / (w - 1));
      Bigarray.Array1.set pixels (off + 1) (y * 255 / (h - 1));
      Bigarray.Array1.set pixels (off + 2) 128
    done
  done;
  let img = Tiff.create_image w h pixels in
  let path = Filename.concat dir name in
  Tiff.write_with_options opts path img;
  (path, w, h)

let test_python_reads_ocaml () =
  let dir = Lazy.force tmpdir in
  (* Write TIFFs with various compressions *)
  let files = [
    write_test_image dir "test_none.tif"
      { compression = Compress_none; tile_size = None; predictor = false };
    write_test_image dir "test_lzw.tif"
      { compression = Compress_lzw; tile_size = None; predictor = false };
    write_test_image dir "test_deflate.tif"
      { compression = Compress_deflate; tile_size = None; predictor = false };
    write_test_image dir "test_packbits.tif"
      { compression = Compress_packbits; tile_size = None; predictor = false };
    write_test_image dir "test_tiled.tif"
      { compression = Compress_lzw; tile_size = Some 32; predictor = false };
  ] in
  (* Also write an RGBA image *)
  let w = 40 and h = 30 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 4) in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let off = (y * w + x) * 4 in
      Bigarray.Array1.set pixels off (x * 255 / (w - 1));
      Bigarray.Array1.set pixels (off + 1) (y * 255 / (h - 1));
      Bigarray.Array1.set pixels (off + 2) 128;
      Bigarray.Array1.set pixels (off + 3) 200
    done
  done;
  let rgba_img = Tiff.create_rgba_image w h pixels in
  let rgba_path = Filename.concat dir "test_rgba.tif" in
  Tiff.write rgba_path rgba_img;
  (* Build Python validation script *)
  let script = Buffer.create 1024 in
  Buffer.add_string script {|
import numpy as np
from PIL import Image
import sys

errors = []

def check_rgb(path, w, h):
    img = Image.open(path)
    if img.size != (w, h):
        errors.append(f"{path}: expected {w}x{h}, got {img.size}")
        return
    arr = np.array(img)
    if arr.shape[2] < 3:
        errors.append(f"{path}: expected RGB, got shape {arr.shape}")
        return
    # Check corner pixels
    expected_r_0_0 = 0
    expected_g_0_0 = 0
    expected_r_99_79 = 255
    expected_g_99_79 = 255
    if abs(int(arr[0, 0, 0]) - expected_r_0_0) > 1:
        errors.append(f"{path}: pixel(0,0) R={arr[0,0,0]} expected ~{expected_r_0_0}")
    if abs(int(arr[0, 0, 1]) - expected_g_0_0) > 1:
        errors.append(f"{path}: pixel(0,0) G={arr[0,0,1]} expected ~{expected_g_0_0}")
    if abs(int(arr[79, 99, 0]) - expected_r_99_79) > 1:
        errors.append(f"{path}: pixel(99,79) R={arr[79,99,0]} expected ~{expected_r_99_79}")
    if abs(int(arr[79, 99, 1]) - expected_g_99_79) > 1:
        errors.append(f"{path}: pixel(99,79) G={arr[79,99,1]} expected ~{expected_g_99_79}")
    if arr[0, 0, 2] != 128:
        errors.append(f"{path}: pixel(0,0) B={arr[0,0,2]} expected 128")

|};
  List.iter (fun (path, _w, _h) ->
    Buffer.add_string script (Printf.sprintf "check_rgb(%S, 100, 80)\n" path)
  ) files;
  Buffer.add_string script (Printf.sprintf {|
# Check RGBA
img = Image.open(%S)
if img.size != (40, 30):
    errors.append(f"RGBA: expected 40x30, got {img.size}")
elif img.mode != "RGBA":
    errors.append(f"RGBA: expected RGBA mode, got {img.mode}")
else:
    arr = np.array(img)
    if arr[0, 0, 3] != 200:
        errors.append(f"RGBA: alpha={arr[0,0,3]} expected 200")

if errors:
    for e in errors:
        print(f"FAIL: {e}", file=sys.stderr)
    sys.exit(1)
else:
    print(f"OK: validated {%d + 1} files")
|} rgba_path (List.length files));
  run_python (Buffer.contents script)

let test_ocaml_reads_python () =
  let dir = Lazy.force tmpdir in
  let u16_path = Filename.concat dir "py_uint16.tif" in
  let f32_path = Filename.concat dir "py_float32.tif" in
  let rgb16_path = Filename.concat dir "py_rgb16.tif" in
  let script = Printf.sprintf {|
import numpy as np
import tifffile

# uint16 single-band
u16 = np.array([[0, 1000, 50000, 65535],
                [100, 200, 300, 400],
                [10000, 20000, 30000, 40000]], dtype=np.uint16)
tifffile.imwrite(%S, u16)

# float32 single-band
f32 = np.array([[0.0, 1523.7, -9999.0, 3.14159],
                [1.0, 2.0, 3.0, 4.0],
                [100.5, 200.25, 300.125, -0.5]], dtype=np.float32)
tifffile.imwrite(%S, f32)

# RGB uint16
W, H = 4, 3
rgb16 = np.zeros((H, W, 3), dtype=np.uint16)
for y in range(H):
    for x in range(W):
        rgb16[y, x, 0] = y * 1000 + x
        rgb16[y, x, 1] = y * 2000 + x * 10
        rgb16[y, x, 2] = 50000
tifffile.imwrite(%S, rgb16, photometric='rgb')

print("OK: generated 3 test TIFFs")
|} u16_path f32_path rgb16_path in
  run_python script;
  (* Read and validate uint16 *)
  let r = Tiff.read_raster u16_path in
  Alcotest.(check int) "u16 width" 4 r.raster_width;
  Alcotest.(check int) "u16 height" 3 r.raster_height;
  Alcotest.(check int) "u16 bands" 1 r.bands;
  Alcotest.(check bool) "u16 format" true (r.sample_format = Tiff.Uint16);
  (match r.data with
   | Tiff.Raster_uint16 arr ->
     let expected = [| 0; 1000; 50000; 65535; 100; 200; 300; 400;
                       10000; 20000; 30000; 40000 |] in
     Array.iteri (fun i v ->
       Alcotest.(check int) (Printf.sprintf "u16[%d]" i) v
         (Bigarray.Array1.get arr i)
     ) expected
   | _ -> Alcotest.fail "expected Raster_uint16");
  (* Read and validate float32 *)
  let r = Tiff.read_raster f32_path in
  Alcotest.(check int) "f32 width" 4 r.raster_width;
  Alcotest.(check int) "f32 bands" 1 r.bands;
  Alcotest.(check bool) "f32 format" true (r.sample_format = Tiff.Float32);
  (match r.data with
   | Tiff.Raster_float32 arr ->
     let expected = [| 0.0; 1523.7; -9999.0; 3.14159;
                       1.0; 2.0; 3.0; 4.0;
                       100.5; 200.25; 300.125; -0.5 |] in
     Array.iteri (fun i v ->
       Alcotest.(check (float 0.01)) (Printf.sprintf "f32[%d]" i) v
         (Bigarray.Array1.get arr i)
     ) expected
   | _ -> Alcotest.fail "expected Raster_float32");
  (* Read and validate RGB uint16 *)
  let r = Tiff.read_raster rgb16_path in
  Alcotest.(check int) "rgb16 width" 4 r.raster_width;
  Alcotest.(check int) "rgb16 bands" 3 r.bands;
  Alcotest.(check bool) "rgb16 format" true (r.sample_format = Tiff.Uint16);
  (match r.data with
   | Tiff.Raster_uint16 arr ->
     (* pixel[0,0] = (0, 0, 50000) *)
     Alcotest.(check int) "rgb16[0,0].r" 0 (Bigarray.Array1.get arr 0);
     Alcotest.(check int) "rgb16[0,0].g" 0 (Bigarray.Array1.get arr 1);
     Alcotest.(check int) "rgb16[0,0].b" 50000 (Bigarray.Array1.get arr 2);
     (* pixel[2,3] = (2*1000+3, 2*2000+3*10, 50000) = (2003, 4030, 50000) *)
     let off = (2 * 4 + 3) * 3 in
     Alcotest.(check int) "rgb16[2,3].r" 2003 (Bigarray.Array1.get arr off);
     Alcotest.(check int) "rgb16[2,3].g" 4030 (Bigarray.Array1.get arr (off + 1));
     Alcotest.(check int) "rgb16[2,3].b" 50000 (Bigarray.Array1.get arr (off + 2))
   | _ -> Alcotest.fail "expected Raster_uint16")

let test_python_lzw_interop () =
  let dir = Lazy.force tmpdir in
  (* Write a large LZW TIFF from OCaml that exercises code-size boundaries *)
  let w = 200 and h = 150 in
  let pixels = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 3) in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let off = (y * w + x) * 3 in
      Bigarray.Array1.set pixels off ((x * 7 + y * 3) mod 256);
      Bigarray.Array1.set pixels (off + 1) ((x * 13 + y * 11) mod 256);
      Bigarray.Array1.set pixels (off + 2) ((x + y) mod 256)
    done
  done;
  let img = Tiff.create_image w h pixels in
  let path = Filename.concat dir "test_lzw_large.tif" in
  Tiff.write_with_options
    { compression = Compress_lzw; tile_size = None; predictor = false }
    path img;
  (* Python reads the LZW file and checks specific pixels *)
  let script = Printf.sprintf {|
import numpy as np
from PIL import Image
import sys

img = Image.open(%S)
arr = np.array(img)
errors = []
if img.size != (200, 150):
    errors.append(f"size: expected 200x150, got {img.size}")

# Check a few pixels match the formula
for y, x in [(0,0), (50,100), (149,199), (75,75)]:
    expected_r = (x * 7 + y * 3) %% 256
    expected_g = (x * 13 + y * 11) %% 256
    expected_b = (x + y) %% 256
    actual = arr[y, x]
    if actual[0] != expected_r or actual[1] != expected_g or actual[2] != expected_b:
        errors.append(f"pixel({x},{y}): expected ({expected_r},{expected_g},{expected_b}), got ({actual[0]},{actual[1]},{actual[2]})")

if errors:
    for e in errors:
        print(f"FAIL: {e}", file=sys.stderr)
    sys.exit(1)
print("OK: LZW interop validated")
|} path in
  run_python script

let () =
  Alcotest.run "tiff" [
    "byte_order", [
      Alcotest.test_case "detect LE" `Quick test_byte_order_le;
      Alcotest.test_case "detect BE" `Quick test_byte_order_be;
      Alcotest.test_case "detect invalid" `Quick test_byte_order_invalid;
      Alcotest.test_case "uint16 LE" `Quick test_uint16_le;
      Alcotest.test_case "uint16 BE" `Quick test_uint16_be;
    ];
    "packbits", [
      Alcotest.test_case "decompress literal" `Quick test_packbits_decompress;
      Alcotest.test_case "decompress repeat" `Quick test_packbits_repeat;
      Alcotest.test_case "roundtrip" `Quick test_packbits_roundtrip;
    ];
    "lzw", [
      Alcotest.test_case "roundtrip" `Quick test_lzw_roundtrip;
    ];
    "deflate", [
      Alcotest.test_case "roundtrip" `Quick test_deflate_roundtrip;
    ];
    "predict", [
      Alcotest.test_case "undo horizontal" `Quick test_predict_horizontal;
      Alcotest.test_case "roundtrip" `Quick test_predict_roundtrip;
    ];
    "sample", [
      Alcotest.test_case "1-bit expand" `Quick test_sample_1bit;
      Alcotest.test_case "4-bit expand" `Quick test_sample_4bit;
      Alcotest.test_case "16 to 8" `Quick test_sample_16_to_8;
    ];
    "color", [
      Alcotest.test_case "grayscale to rgb" `Quick test_color_grayscale;
    ];
    "image", [
      Alcotest.test_case "create" `Quick test_create_image;
      Alcotest.test_case "create bad size" `Quick test_create_image_bad_size;
      Alcotest.test_case "get/set pixel" `Quick test_get_set_pixel;
      Alcotest.test_case "get/set pixel rgba" `Quick test_get_set_pixel_rgba;
      Alcotest.test_case "bounds check" `Quick test_pixel_bounds;
    ];
    "roundtrip", [
      Alcotest.test_case "RGB no compression" `Quick test_roundtrip_rgb_no_compression;
      Alcotest.test_case "RGB LZW" `Quick test_roundtrip_rgb_lzw;
      Alcotest.test_case "RGB PackBits" `Quick test_roundtrip_rgb_packbits;
      Alcotest.test_case "RGB Deflate" `Quick test_roundtrip_rgb_deflate;
      Alcotest.test_case "RGBA" `Quick test_roundtrip_rgba;
      Alcotest.test_case "tiled" `Quick test_roundtrip_tiled;
    ];
    "geokeys", [
      Alcotest.test_case "parse directory" `Quick test_geokeys_parse;
    ];
    "geotransform", [
      Alcotest.test_case "tiepoint+scale" `Quick test_geotransform_tiepoint_scale;
      Alcotest.test_case "roundtrip" `Quick test_geotransform_roundtrip;
    ];
    "icc", [
      Alcotest.test_case "from/to bytes" `Quick test_icc_tiff;
    ];
    "raster", [
      Alcotest.test_case "uint8 roundtrip" `Quick test_raster_uint8_roundtrip;
      Alcotest.test_case "uint16" `Quick test_raster_uint16;
      Alcotest.test_case "float32" `Quick test_raster_float32;
    ];
    "python_interop", [
      Alcotest.test_case "python reads ocaml" `Slow
        (skip_without_python test_python_reads_ocaml);
      Alcotest.test_case "ocaml reads python" `Slow
        (skip_without_python test_ocaml_reads_python);
      Alcotest.test_case "LZW interop" `Slow
        (skip_without_python test_python_lzw_interop);
    ];
  ]
