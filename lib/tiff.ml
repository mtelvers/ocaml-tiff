(** Pure OCaml TIFF/GeoTIFF library for reading and writing TIFF files.

    Supported features:
    - Strip-based and tile-based images
    - Compression: None, PackBits, LZW, Deflate
    - Photometric: WhiteIsZero, BlackIsZero, RGB, Palette, CMYK, YCbCr
    - Bit depths: 1, 2, 4, 8, 16 (normalized to 8-bit output)
    - Horizontal differencing predictor
    - Multi-IFD files (COG overviews)
    - GeoTIFF metadata (GeoKeys, coordinate transformations)
    - ICC color profiles *)

(* ---- Exposed internal modules ---- *)

module Byte_order = Byte_order
module Ifd = Ifd
module Tags = Tags
module Compression = Compression
module Packbits = Packbits
module Lzw = Lzw
module Deflate = Deflate
module Predict = Predict
module Strips = Strips
module Tiles = Tiles
module Color = Color
module Sample = Sample
module Geokeys = Geokeys
module Geotransform = Geotransform
module Icc_tiff = Icc_tiff

(* ---- Core types ---- *)

type pixel_format = RGB24 | RGBA32

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type image = {
  width : int;
  height : int;
  pixels : pixel_data;
  pixel_format : pixel_format;
  icc_profile : Icc_tiff.t option;
}

(* ---- GeoTIFF types ---- *)

type geo_metadata = {
  geokeys : Geokeys.t option;
  transform : Geotransform.t;
  double_params : float list;
  ascii_params : string;
}

type geo_image = {
  image : image;
  geo : geo_metadata;
}

(* ---- Raster types ---- *)

type sample_format = Uint8 | Uint16 | Int16 | Int32 | Uint32 | Float32 | Float64

type raster_data =
  | Raster_uint8   of (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Raster_uint16  of (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Raster_int16   of (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Raster_int32   of (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Raster_float32 of (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  | Raster_float64 of (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

type raster = {
  raster_width : int;
  raster_height : int;
  bands : int;
  sample_format : sample_format;
  data : raster_data;
  nodata : float option;
}

type geo_raster = {
  raster : raster;
  geo : geo_metadata;
}

(* ---- IFD info type ---- *)

type ifd_info = {
  ifd_width : int;
  ifd_height : int;
  ifd_compression : int;
  ifd_samples_per_pixel : int;
  ifd_bits_per_sample : int;
  ifd_is_tiled : bool;
  ifd_new_subfile_type : int;
  ifd_sample_format : int;
}

(* ---- Encoding options ---- *)

type compression_option =
  | Compress_none
  | Compress_packbits
  | Compress_lzw
  | Compress_deflate

type encode_options = {
  compression : compression_option;
  tile_size : int option;
  predictor : bool;
}

let default_encode_options = {
  compression = Compress_lzw;
  tile_size = None;
  predictor = false;
}

(* ---- Internal helpers ---- *)

let bigarray_init kind n f =
  let arr = Bigarray.Array1.create kind Bigarray.c_layout n in
  for i = 0 to n - 1 do
    Bigarray.Array1.set arr i (f i)
  done;
  arr

let bytes_of_pixel_data (px : pixel_data) =
  Bytes.init (Bigarray.Array1.dim px) (fun i -> Char.chr (Bigarray.Array1.get px i))

let pixel_data_of_bytes buf =
  bigarray_init Bigarray.int8_unsigned (Bytes.length buf)
    (fun i -> Bytes.get_uint8 buf i)

let get_tag_int order buf ifd ~ifd_offset:_ tag default =
  match Ifd.find_entry_index ifd tag with
  | Some (entry, _idx) -> Ifd.get_required_int order buf entry
  | None -> default

let get_tag_int_required order buf ifd ~ifd_offset:_ tag name =
  match Ifd.find_entry_index ifd tag with
  | Some (entry, _idx) -> Ifd.get_required_int order buf entry
  | None -> failwith (Printf.sprintf "TIFF: missing required tag %s" name)

let get_tag_ascii order buf ifd ~ifd_offset tag =
  match Ifd.find_entry_index ifd tag with
  | Some (entry, idx) ->
    Some (Ifd.get_ascii_value order buf entry ~ifd_offset ~entry_index:idx)
  | None -> None

let read_file filename f =
  let ic = open_in_bin filename in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    f buf)

(* ---- Header parsing ---- *)

let parse_header buf =
  if Bytes.length buf < 8 then failwith "TIFF: file too short";
  let order = Byte_order.detect buf in
  let magic = Byte_order.get_uint16 order buf 2 in
  if magic <> 42 then failwith "TIFF: invalid magic number (expected 42)";
  let first_ifd_offset = Byte_order.get_uint32 order buf 4 in
  (order, first_ifd_offset)

let with_nth_ifd buf n f =
  let order, first_ifd_offset = parse_header buf in
  let ifds = Ifd.parse_all order buf first_ifd_offset in
  if n >= List.length ifds then
    failwith (Printf.sprintf "TIFF: IFD index %d out of range (file has %d IFDs)"
      n (List.length ifds));
  let ifd = List.nth ifds n in
  let rec walk_to_offset offset remaining =
    if remaining = 0 then offset
    else
      let parsed = Ifd.parse order buf offset in
      walk_to_offset parsed.next_ifd_offset (remaining - 1)
  in
  let ifd_offset = walk_to_offset first_ifd_offset n in
  f order buf ifd ~ifd_offset

(* ---- Read GeoTIFF metadata from an IFD ---- *)

let read_geo_metadata order buf ifd ~ifd_offset =
  (* GeoKeyDirectory *)
  let geokeys =
    match Ifd.find_entry_index ifd Tags.geo_key_directory with
    | Some (entry, idx) ->
      let shorts = Ifd.get_short_values order buf entry
        ~ifd_offset ~entry_index:idx in
      Geokeys.parse_directory shorts
    | None -> None
  in
  (* GeoDoubleParams *)
  let double_params =
    match Ifd.find_entry_index ifd Tags.geo_double_params with
    | Some (entry, idx) ->
      let arr = Ifd.get_double_values order buf entry
        ~ifd_offset ~entry_index:idx in
      Array.to_list arr
    | None -> []
  in
  (* GeoAsciiParams *)
  let ascii_params =
    match Ifd.find_entry_index ifd Tags.geo_ascii_params with
    | Some (entry, idx) ->
      Ifd.get_ascii_value order buf entry ~ifd_offset ~entry_index:idx
    | None -> ""
  in
  (* Tiepoints *)
  let tiepoints =
    match Ifd.find_entry_index ifd Tags.model_tiepoint with
    | Some (entry, idx) ->
      let arr = Ifd.get_double_values order buf entry
        ~ifd_offset ~entry_index:idx in
      Geotransform.parse_tiepoints arr
    | None -> []
  in
  (* Pixel scale *)
  let pixel_scale =
    match Ifd.find_entry_index ifd Tags.model_pixel_scale with
    | Some (entry, idx) ->
      let arr = Ifd.get_double_values order buf entry
        ~ifd_offset ~entry_index:idx in
      Geotransform.parse_pixel_scale arr
    | None -> None
  in
  (* Transformation matrix *)
  let transformation =
    match Ifd.find_entry_index ifd Tags.model_transformation with
    | Some (entry, idx) ->
      let arr = Ifd.get_double_values order buf entry
        ~ifd_offset ~entry_index:idx in
      Geotransform.parse_transformation arr
    | None -> None
  in
  let transform = { Geotransform.tiepoints; pixel_scale; transformation } in
  { geokeys; transform; double_params; ascii_params }

(* ---- Read ICC profile ---- *)

let read_icc_profile _order buf ifd ~ifd_offset =
  match Ifd.find_entry_index ifd Tags.icc_profile with
  | Some (entry, idx) ->
    let data = Ifd.get_bytes buf entry ~ifd_offset ~entry_index:idx in
    Some (Icc_tiff.from_bytes data)
  | None -> None

(* ---- Raster helpers ---- *)

let determine_sample_format ~sample_format_tag ~bits_per_sample =
  match sample_format_tag, bits_per_sample with
  | 1, 8  -> Uint8
  | 1, 16 -> Uint16
  | 1, 32 -> Uint32
  | 2, 16 -> Int16
  | 2, 32 -> Int32
  | 3, 32 -> Float32
  | 3, 64 -> Float64
  | sf, bps ->
    failwith (Printf.sprintf
      "TIFF: unsupported sample format %d with %d bits per sample" sf bps)

let bytes_per_sample_of_format = function
  | Uint8 -> 1
  | Uint16 | Int16 -> 2
  | Int32 | Uint32 | Float32 -> 4
  | Float64 -> 8

let deinterleave_planar_to_chunky ~width ~height ~samples_per_pixel ~bps data =
  let plane_size = width * height * bps in
  let out = Bytes.create (width * height * samples_per_pixel * bps) in
  for p = 0 to samples_per_pixel - 1 do
    for i = 0 to width * height - 1 do
      let src_off = p * plane_size + i * bps in
      let dst_off = (i * samples_per_pixel + p) * bps in
      Bytes.blit data src_off out dst_off bps
    done
  done;
  out

let bytes_to_raster_data order fmt num_samples data =
  match fmt with
  | Uint8 ->
    Raster_uint8 (bigarray_init Bigarray.int8_unsigned num_samples
      (fun i -> Bytes.get_uint8 data i))
  | Uint16 ->
    Raster_uint16 (bigarray_init Bigarray.int16_unsigned num_samples
      (fun i -> Byte_order.get_uint16 order data (i * 2)))
  | Int16 ->
    Raster_int16 (bigarray_init Bigarray.int16_signed num_samples
      (fun i -> Byte_order.get_int16 order data (i * 2)))
  | Int32 | Uint32 ->
    Raster_int32 (bigarray_init Bigarray.int32 num_samples
      (fun i -> Byte_order.get_int32 order data (i * 4)))
  | Float32 ->
    Raster_float32 (bigarray_init Bigarray.float32 num_samples
      (fun i -> Byte_order.get_float order data (i * 4)))
  | Float64 ->
    Raster_float64 (bigarray_init Bigarray.float64 num_samples
      (fun i -> Byte_order.get_double order data (i * 8)))

(* ---- Read image data from a single IFD ---- *)

let read_image_from_ifd order buf ifd ~ifd_offset =
  let width = get_tag_int_required order buf ifd ~ifd_offset
    Tags.image_width "ImageWidth" in
  let height = get_tag_int_required order buf ifd ~ifd_offset
    Tags.image_length "ImageLength" in
  let bits_per_sample = get_tag_int order buf ifd ~ifd_offset
    Tags.bits_per_sample 1 in
  let compression = get_tag_int order buf ifd ~ifd_offset
    Tags.compression 1 in
  let photometric = get_tag_int order buf ifd ~ifd_offset
    Tags.photometric_interpretation 1 in
  let samples_per_pixel = get_tag_int order buf ifd ~ifd_offset
    Tags.samples_per_pixel 1 in
  let predictor = get_tag_int order buf ifd ~ifd_offset
    Tags.predictor 1 in
  let _planar = get_tag_int order buf ifd ~ifd_offset
    Tags.planar_configuration 1 in
  (* Check for extra samples *)
  let extra_samples =
    match Ifd.find_entry_index ifd Tags.extra_samples with
    | Some (entry, idx) ->
      Some (Ifd.get_int_values order buf entry ~ifd_offset ~entry_index:idx)
    | None -> None
  in
  (* Check if tiled or stripped *)
  let is_tiled = Ifd.find_entry ifd Tags.tile_width <> None in
  let raw_data =
    if is_tiled then begin
      let tile_width = get_tag_int_required order buf ifd ~ifd_offset
        Tags.tile_width "TileWidth" in
      let tile_height = get_tag_int_required order buf ifd ~ifd_offset
        Tags.tile_length "TileLength" in
      Tiles.read_tiles order buf ifd ~ifd_offset ~compression ~predictor
        ~width ~height ~tile_width ~tile_height
        ~samples_per_pixel ~bits_per_sample
    end else begin
      Strips.read_strips order buf ifd ~ifd_offset ~compression ~predictor
        ~width ~samples_per_pixel ~bits_per_sample
    end
  in
  (* Normalize bit depth to 8-bit *)
  let normalized = Sample.normalize ~bits_per_sample ~width ~height
    ~samples_per_pixel raw_data in
  (* Handle planar configuration *)
  let planar = get_tag_int order buf ifd ~ifd_offset
    Tags.planar_configuration 1 in
  let interleaved =
    if planar = Tags.planar_planar && samples_per_pixel > 1 then
      deinterleave_planar_to_chunky ~width ~height ~samples_per_pixel ~bps:1 normalized
    else
      normalized
  in
  (* Convert to RGB/RGBA *)
  let color_result = Color.convert ~photometric ~samples_per_pixel
    ~extra_samples order buf ifd ~ifd_offset ~width ~height interleaved in
  (* Read ICC profile *)
  let icc_profile = read_icc_profile order buf ifd ~ifd_offset in
  match color_result with
  | Color.RGB data ->
    let pixels = pixel_data_of_bytes data in
    { width; height; pixels; pixel_format = RGB24; icc_profile }
  | Color.RGBA data ->
    let pixels = pixel_data_of_bytes data in
    { width; height; pixels; pixel_format = RGBA32; icc_profile }

(* ---- Public reading functions ---- *)

let read_bytes_ifd buf n =
  with_nth_ifd buf n read_image_from_ifd

let read_bytes data =
  read_bytes_ifd data 0

let read filename =
  read_file filename read_bytes

let read_ifd filename n =
  read_file filename (fun buf -> read_bytes_ifd buf n)

(* ---- GeoTIFF reading ---- *)

let read_geo_bytes_ifd buf n =
  with_nth_ifd buf n (fun order buf ifd ~ifd_offset ->
    let image = read_image_from_ifd order buf ifd ~ifd_offset in
    let geo = read_geo_metadata order buf ifd ~ifd_offset in
    { image; geo })

let read_geo_bytes data =
  read_geo_bytes_ifd data 0

let read_geo filename =
  read_file filename read_geo_bytes

let read_geo_ifd filename n =
  read_file filename (fun buf -> read_geo_bytes_ifd buf n)

(* ---- Read raster data from a single IFD ---- *)

let read_raster_from_ifd order buf ifd ~ifd_offset =
  let width = get_tag_int_required order buf ifd ~ifd_offset
    Tags.image_width "ImageWidth" in
  let height = get_tag_int_required order buf ifd ~ifd_offset
    Tags.image_length "ImageLength" in
  let bits_per_sample = get_tag_int order buf ifd ~ifd_offset
    Tags.bits_per_sample 1 in
  let compression = get_tag_int order buf ifd ~ifd_offset
    Tags.compression 1 in
  let samples_per_pixel = get_tag_int order buf ifd ~ifd_offset
    Tags.samples_per_pixel 1 in
  let predictor = get_tag_int order buf ifd ~ifd_offset
    Tags.predictor 1 in
  let sample_format_tag = get_tag_int order buf ifd ~ifd_offset
    Tags.sample_format 1 in
  let fmt = determine_sample_format ~sample_format_tag ~bits_per_sample in
  let bps = bytes_per_sample_of_format fmt in
  (* Read NoData *)
  let nodata =
    match get_tag_ascii order buf ifd ~ifd_offset Tags.gdal_nodata with
    | Some s -> (try Some (float_of_string (String.trim s)) with _ -> None)
    | None -> None
  in
  (* Check if tiled or stripped *)
  let is_tiled = Ifd.find_entry ifd Tags.tile_width <> None in
  let raw_data =
    if is_tiled then begin
      let tile_width = get_tag_int_required order buf ifd ~ifd_offset
        Tags.tile_width "TileWidth" in
      let tile_height = get_tag_int_required order buf ifd ~ifd_offset
        Tags.tile_length "TileLength" in
      Tiles.read_tiles order buf ifd ~ifd_offset ~compression ~predictor
        ~width ~height ~tile_width ~tile_height
        ~samples_per_pixel ~bits_per_sample
    end else begin
      Strips.read_strips order buf ifd ~ifd_offset ~compression ~predictor
        ~width ~samples_per_pixel ~bits_per_sample
    end
  in
  (* Handle planar configuration *)
  let planar = get_tag_int order buf ifd ~ifd_offset
    Tags.planar_configuration 1 in
  let interleaved =
    if planar = Tags.planar_planar && samples_per_pixel > 1 then
      deinterleave_planar_to_chunky ~width ~height ~samples_per_pixel ~bps raw_data
    else
      raw_data
  in
  let num_samples = width * height * samples_per_pixel in
  let data = bytes_to_raster_data order fmt num_samples interleaved in
  { raster_width = width; raster_height = height; bands = samples_per_pixel;
    sample_format = fmt; data; nodata }

(* ---- Public raster reading functions ---- *)

let read_raster_bytes_ifd buf n =
  with_nth_ifd buf n read_raster_from_ifd

let read_raster_bytes data =
  read_raster_bytes_ifd data 0

let read_raster filename =
  read_file filename read_raster_bytes

let read_raster_ifd filename n =
  read_file filename (fun buf -> read_raster_bytes_ifd buf n)

(* ---- GeoRaster reading ---- *)

let read_geo_raster_bytes_ifd buf n =
  with_nth_ifd buf n (fun order buf ifd ~ifd_offset ->
    let raster = read_raster_from_ifd order buf ifd ~ifd_offset in
    let geo = read_geo_metadata order buf ifd ~ifd_offset in
    { raster; geo })

let read_geo_raster_bytes data =
  read_geo_raster_bytes_ifd data 0

let read_geo_raster filename =
  read_file filename read_geo_raster_bytes

let read_geo_raster_ifd filename n =
  read_file filename (fun buf -> read_geo_raster_bytes_ifd buf n)

(* ---- File inspection ---- *)

let ifd_count_bytes buf =
  let order, first_ifd_offset = parse_header buf in
  let ifds = Ifd.parse_all order buf first_ifd_offset in
  List.length ifds

let ifd_count filename =
  read_file filename ifd_count_bytes

let ifd_info_bytes buf =
  let order, first_ifd_offset = parse_header buf in
  let ifds = Ifd.parse_all order buf first_ifd_offset in
  let rec collect offset = function
    | [] -> []
    | ifd :: rest ->
      let width = get_tag_int order buf ifd ~ifd_offset:offset
        Tags.image_width 0 in
      let height = get_tag_int order buf ifd ~ifd_offset:offset
        Tags.image_length 0 in
      let compression = get_tag_int order buf ifd ~ifd_offset:offset
        Tags.compression 1 in
      let samples_per_pixel = get_tag_int order buf ifd ~ifd_offset:offset
        Tags.samples_per_pixel 1 in
      let bits_per_sample = get_tag_int order buf ifd ~ifd_offset:offset
        Tags.bits_per_sample 1 in
      let is_tiled = Ifd.find_entry ifd Tags.tile_width <> None in
      let new_subfile_type = get_tag_int order buf ifd ~ifd_offset:offset
        Tags.new_subfile_type 0 in
      let sample_format_val = get_tag_int order buf ifd ~ifd_offset:offset
        Tags.sample_format 1 in
      let info = {
        ifd_width = width;
        ifd_height = height;
        ifd_compression = compression;
        ifd_samples_per_pixel = samples_per_pixel;
        ifd_bits_per_sample = bits_per_sample;
        ifd_is_tiled = is_tiled;
        ifd_new_subfile_type = new_subfile_type;
        ifd_sample_format = sample_format_val;
      } in
      let parsed = Ifd.parse order buf offset in
      info :: collect parsed.next_ifd_offset rest
  in
  collect first_ifd_offset ifds

let ifd_info filename =
  read_file filename ifd_info_bytes

(* ---- Image creation ---- *)

let create_image width height pixels =
  let expected = width * height * 3 in
  if Bigarray.Array1.dim pixels <> expected then
    failwith (Printf.sprintf "TIFF: pixel data size mismatch (expected %d, got %d)"
      expected (Bigarray.Array1.dim pixels));
  { width; height; pixels; pixel_format = RGB24; icc_profile = None }

let create_rgba_image width height pixels =
  let expected = width * height * 4 in
  if Bigarray.Array1.dim pixels <> expected then
    failwith (Printf.sprintf "TIFF: pixel data size mismatch (expected %d, got %d)"
      expected (Bigarray.Array1.dim pixels));
  { width; height; pixels; pixel_format = RGBA32; icc_profile = None }

let create_image_with_icc width height pixels icc =
  let expected = width * height * 3 in
  if Bigarray.Array1.dim pixels <> expected then
    failwith (Printf.sprintf "TIFF: pixel data size mismatch (expected %d, got %d)"
      expected (Bigarray.Array1.dim pixels));
  { width; height; pixels; pixel_format = RGB24; icc_profile = Some icc }

(* ---- Pixel access ---- *)

let get_pixel img x y =
  if x < 0 || x >= img.width || y < 0 || y >= img.height then
    failwith "TIFF: pixel coordinates out of bounds";
  let bpp = match img.pixel_format with RGB24 -> 3 | RGBA32 -> 4 in
  let off = (y * img.width + x) * bpp in
  let r = Bigarray.Array1.get img.pixels off in
  let g = Bigarray.Array1.get img.pixels (off + 1) in
  let b = Bigarray.Array1.get img.pixels (off + 2) in
  (r, g, b)

let get_pixel_rgba img x y =
  if x < 0 || x >= img.width || y < 0 || y >= img.height then
    failwith "TIFF: pixel coordinates out of bounds";
  match img.pixel_format with
  | RGBA32 ->
    let off = (y * img.width + x) * 4 in
    let r = Bigarray.Array1.get img.pixels off in
    let g = Bigarray.Array1.get img.pixels (off + 1) in
    let b = Bigarray.Array1.get img.pixels (off + 2) in
    let a = Bigarray.Array1.get img.pixels (off + 3) in
    (r, g, b, a)
  | RGB24 ->
    let off = (y * img.width + x) * 3 in
    let r = Bigarray.Array1.get img.pixels off in
    let g = Bigarray.Array1.get img.pixels (off + 1) in
    let b = Bigarray.Array1.get img.pixels (off + 2) in
    (r, g, b, 255)

let set_pixel img x y r g b =
  if x < 0 || x >= img.width || y < 0 || y >= img.height then
    failwith "TIFF: pixel coordinates out of bounds";
  let bpp = match img.pixel_format with RGB24 -> 3 | RGBA32 -> 4 in
  let off = (y * img.width + x) * bpp in
  Bigarray.Array1.set img.pixels off r;
  Bigarray.Array1.set img.pixels (off + 1) g;
  Bigarray.Array1.set img.pixels (off + 2) b;
  if bpp = 4 then Bigarray.Array1.set img.pixels (off + 3) 255

let set_pixel_rgba img x y r g b a =
  if x < 0 || x >= img.width || y < 0 || y >= img.height then
    failwith "TIFF: pixel coordinates out of bounds";
  match img.pixel_format with
  | RGBA32 ->
    let off = (y * img.width + x) * 4 in
    Bigarray.Array1.set img.pixels off r;
    Bigarray.Array1.set img.pixels (off + 1) g;
    Bigarray.Array1.set img.pixels (off + 2) b;
    Bigarray.Array1.set img.pixels (off + 3) a
  | RGB24 ->
    failwith "TIFF: set_pixel_rgba requires RGBA32 image"

(* ---- GeoTIFF utilities ---- *)

let raster_to_model (geo_img : geo_image) rx ry =
  Geotransform.raster_to_model geo_img.geo.transform rx ry

let model_to_raster (geo_img : geo_image) mx my =
  Geotransform.model_to_raster geo_img.geo.transform mx my

let bounding_box (geo_img : geo_image) =
  Geotransform.bounding_box geo_img.geo.transform
    ~width:geo_img.image.width ~height:geo_img.image.height

let epsg_code (geo_img : geo_image) =
  match geo_img.geo.geokeys with
  | None -> None
  | Some gk ->
    match Geokeys.projected_cs_type gk with
    | Some code when code <> 0 && code <> 32767 -> Some code
    | _ ->
      match Geokeys.geographic_type gk with
      | Some code when code <> 0 && code <> 32767 -> Some code
      | _ -> None

(* ---- Writing ---- *)

(* IFD entry builder: tag, type, count, and either inline value or external bytes *)
type ifd_entry_value =
  | Inline_long of int         (* single LONG value, stored inline *)
  | Inline_short of int        (* single SHORT value, stored inline *)
  | External_shorts of int array  (* array of SHORTs, stored externally *)
  | External_longs of int array   (* array of LONGs, stored externally *)
  | External_rational of int * int  (* numerator, denominator *)

type ifd_builder_entry = {
  ib_tag : int;
  ib_type : int;
  ib_count : int;
  ib_value : ifd_entry_value;
}

let write_ifd_and_data order buf ifd_offset entries =
  let num_entries = List.length entries in
  let sorted = List.sort (fun a b -> compare a.ib_tag b.ib_tag) entries in
  Byte_order.set_uint16 order buf ifd_offset num_entries;
  let ext_data_start = ifd_offset + 2 + num_entries * 12 + 4 in
  let write_entry (i, ext_pos) entry =
    let entry_off = ifd_offset + 2 + i * 12 in
    Byte_order.set_uint16 order buf entry_off entry.ib_tag;
    Byte_order.set_uint16 order buf (entry_off + 2) entry.ib_type;
    Byte_order.set_uint32 order buf (entry_off + 4) entry.ib_count;
    let ext_pos = match entry.ib_value with
      | Inline_long v ->
        Byte_order.set_uint32 order buf (entry_off + 8) v;
        ext_pos
      | Inline_short v ->
        Byte_order.set_uint16 order buf (entry_off + 8) v;
        Byte_order.set_uint16 order buf (entry_off + 10) 0;
        ext_pos
      | External_shorts arr ->
        let total_bytes = Array.length arr * 2 in
        if total_bytes <= 4 then begin
          Array.iteri (fun j v ->
            Byte_order.set_uint16 order buf (entry_off + 8 + j * 2) v
          ) arr;
          ext_pos
        end else begin
          Byte_order.set_uint32 order buf (entry_off + 8) ext_pos;
          Array.iteri (fun j v ->
            Byte_order.set_uint16 order buf (ext_pos + j * 2) v
          ) arr;
          ext_pos + total_bytes
        end
      | External_longs arr ->
        let total_bytes = Array.length arr * 4 in
        if total_bytes <= 4 then begin
          Byte_order.set_uint32 order buf (entry_off + 8) arr.(0);
          ext_pos
        end else begin
          Byte_order.set_uint32 order buf (entry_off + 8) ext_pos;
          Array.iteri (fun j v ->
            Byte_order.set_uint32 order buf (ext_pos + j * 4) v
          ) arr;
          ext_pos + total_bytes
        end
      | External_rational (num, den) ->
        Byte_order.set_uint32 order buf (entry_off + 8) ext_pos;
        Byte_order.set_uint32 order buf ext_pos num;
        Byte_order.set_uint32 order buf (ext_pos + 4) den;
        ext_pos + 8
    in
    (i + 1, ext_pos)
  in
  let _, final_ext_pos = List.fold_left write_entry (0, ext_data_start) sorted in
  let next_ifd_off = ifd_offset + 2 + num_entries * 12 in
  Byte_order.set_uint32 order buf next_ifd_off 0;
  final_ext_pos

let calc_external_size entries =
  List.fold_left (fun acc entry ->
    match entry.ib_value with
    | Inline_long _ | Inline_short _ -> acc
    | External_shorts arr ->
      let total = Array.length arr * 2 in
      if total <= 4 then acc else acc + total
    | External_longs arr ->
      let total = Array.length arr * 4 in
      if total <= 4 then acc else acc + total
    | External_rational _ -> acc + 8
  ) 0 entries

let cumulative_offsets start datas =
  let rev_offsets, total =
    Array.fold_left (fun (acc, off) d ->
      (off :: acc, off + Bytes.length d)
    ) ([], start) datas
  in
  (Array.of_list (List.rev rev_offsets), total)

(* Build IFD entries and write a TIFF file *)
let write_image_to_bytes opts img =
  let order = Byte_order.LE in
  let bpp = match img.pixel_format with RGB24 -> 3 | RGBA32 -> 4 in
  let samples_per_pixel = bpp in
  let photometric = Tags.photometric_rgb in
  let pixel_bytes = bytes_of_pixel_data img.pixels in
  let use_predictor = opts.predictor &&
    (opts.compression = Compress_lzw || opts.compression = Compress_deflate) in
  let predictor_tag = if use_predictor then Tags.predictor_horizontal
    else Tags.predictor_none in
  let compression_opt = match opts.compression with
    | Compress_none -> Compression.Compress_none
    | Compress_packbits -> Compression.Compress_packbits
    | Compress_lzw -> Compression.Compress_lzw
    | Compress_deflate -> Compression.Compress_deflate
  in
  let compression_tag_val = Compression.compression_tag compression_opt in
  let has_alpha = bpp = 4 in
  let bps_array = Array.make samples_per_pixel 8 in
  match opts.tile_size with
  | Some tile_sz ->
    let tiles_across = (img.width + tile_sz - 1) / tile_sz in
    let tiles_down = (img.height + tile_sz - 1) / tile_sz in
    let num_tiles = tiles_across * tiles_down in
    let row_bytes = img.width * bpp in
    let tile_row_bytes = tile_sz * bpp in
    let tile_datas = Array.init num_tiles (fun idx ->
      let tx = idx mod tiles_across in
      let ty = idx / tiles_across in
      let x0 = tx * tile_sz in
      let y0 = ty * tile_sz in
      let copy_w = min tile_sz (img.width - x0) in
      let copy_h = min tile_sz (img.height - y0) in
      let tile_buf = Bytes.make (tile_sz * tile_row_bytes) '\000' in
      for row = 0 to copy_h - 1 do
        let src_off = (y0 + row) * row_bytes + x0 * bpp in
        let dst_off = row * tile_row_bytes in
        Bytes.blit pixel_bytes src_off tile_buf dst_off (copy_w * bpp)
      done;
      let to_compress =
        if use_predictor then
          Predict.apply_horizontal ~order ~width:tile_sz ~samples_per_pixel
            ~bits_per_sample:8 tile_buf
        else tile_buf
      in
      Compression.compress ~compression:compression_opt to_compress
    ) in
    (* We need to know where tile data starts to fill in offsets.
       Build entries with placeholder offsets, compute layout, then fix up. *)
    let tile_byte_counts = Array.init num_tiles (fun i ->
      Bytes.length tile_datas.(i)
    ) in
    (* Build IFD entries (offsets are placeholders, fixed after layout) *)
    let entries = [
      { ib_tag = Tags.image_width; ib_type = Tags.type_long; ib_count = 1;
        ib_value = Inline_long img.width };
      { ib_tag = Tags.image_length; ib_type = Tags.type_long; ib_count = 1;
        ib_value = Inline_long img.height };
      { ib_tag = Tags.bits_per_sample; ib_type = Tags.type_short;
        ib_count = samples_per_pixel; ib_value = External_shorts bps_array };
      { ib_tag = Tags.compression; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short compression_tag_val };
      { ib_tag = Tags.photometric_interpretation; ib_type = Tags.type_short;
        ib_count = 1; ib_value = Inline_short photometric };
      { ib_tag = Tags.samples_per_pixel; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short samples_per_pixel };
      { ib_tag = Tags.tile_width; ib_type = Tags.type_long; ib_count = 1;
        ib_value = Inline_long tile_sz };
      { ib_tag = Tags.tile_length; ib_type = Tags.type_long; ib_count = 1;
        ib_value = Inline_long tile_sz };
      (* Placeholder offsets - will be recalculated *)
      { ib_tag = Tags.tile_offsets; ib_type = Tags.type_long;
        ib_count = num_tiles; ib_value = External_longs (Array.make num_tiles 0) };
      { ib_tag = Tags.tile_byte_counts; ib_type = Tags.type_long;
        ib_count = num_tiles; ib_value = External_longs tile_byte_counts };
      { ib_tag = Tags.sample_format; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short Tags.sample_format_uint };
    ] in
    let entries = if use_predictor then
      { ib_tag = Tags.predictor; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short predictor_tag } :: entries
    else entries in
    let entries = if has_alpha then
      { ib_tag = Tags.extra_samples; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short Tags.extra_unassoc_alpha } :: entries
    else entries in
    let num_entries = List.length entries in
    let ifd_offset = 8 in
    let ifd_size = 2 + num_entries * 12 + 4 in
    let ext_size = calc_external_size entries in
    let data_start = ifd_offset + ifd_size + ext_size in
    let tile_offsets, total_size = cumulative_offsets data_start tile_datas in
    let entries = List.map (fun e ->
      if e.ib_tag = Tags.tile_offsets then
        { e with ib_value = External_longs tile_offsets }
      else e
    ) entries in
    let buf = Bytes.make total_size '\000' in
    Bytes.set_uint8 buf 0 0x49;
    Bytes.set_uint8 buf 1 0x49;
    Byte_order.set_uint16 order buf 2 42;
    Byte_order.set_uint32 order buf 4 ifd_offset;
    let _ = write_ifd_and_data order buf ifd_offset entries in
    Array.iteri (fun i data ->
      Bytes.blit data 0 buf tile_offsets.(i) (Bytes.length data)
    ) tile_datas;
    buf
  | None ->
    let rows_per_strip = 64 in
    let num_strips = (img.height + rows_per_strip - 1) / rows_per_strip in
    let row_bytes = img.width * bpp in
    let strip_datas = Array.init num_strips (fun i ->
      let y0 = i * rows_per_strip in
      let strip_rows = min rows_per_strip (img.height - y0) in
      let strip_size = strip_rows * row_bytes in
      let strip_buf = Bytes.sub pixel_bytes (y0 * row_bytes) strip_size in
      let to_compress =
        if use_predictor then
          Predict.apply_horizontal ~order ~width:img.width ~samples_per_pixel
            ~bits_per_sample:8 strip_buf
        else strip_buf
      in
      Compression.compress ~compression:compression_opt to_compress
    ) in
    let strip_byte_counts = Array.init num_strips (fun i ->
      Bytes.length strip_datas.(i)
    ) in
    let entries = [
      { ib_tag = Tags.image_width; ib_type = Tags.type_long; ib_count = 1;
        ib_value = Inline_long img.width };
      { ib_tag = Tags.image_length; ib_type = Tags.type_long; ib_count = 1;
        ib_value = Inline_long img.height };
      { ib_tag = Tags.bits_per_sample; ib_type = Tags.type_short;
        ib_count = samples_per_pixel; ib_value = External_shorts bps_array };
      { ib_tag = Tags.compression; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short compression_tag_val };
      { ib_tag = Tags.photometric_interpretation; ib_type = Tags.type_short;
        ib_count = 1; ib_value = Inline_short photometric };
      { ib_tag = Tags.samples_per_pixel; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short samples_per_pixel };
      { ib_tag = Tags.rows_per_strip; ib_type = Tags.type_long; ib_count = 1;
        ib_value = Inline_long rows_per_strip };
      (* Placeholder offsets *)
      { ib_tag = Tags.strip_offsets; ib_type = Tags.type_long;
        ib_count = num_strips; ib_value = External_longs (Array.make num_strips 0) };
      { ib_tag = Tags.strip_byte_counts; ib_type = Tags.type_long;
        ib_count = num_strips; ib_value = External_longs strip_byte_counts };
      { ib_tag = Tags.x_resolution; ib_type = Tags.type_rational; ib_count = 1;
        ib_value = External_rational (72, 1) };
      { ib_tag = Tags.y_resolution; ib_type = Tags.type_rational; ib_count = 1;
        ib_value = External_rational (72, 1) };
      { ib_tag = Tags.sample_format; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short Tags.sample_format_uint };
    ] in
    let entries = if use_predictor then
      { ib_tag = Tags.predictor; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short predictor_tag } :: entries
    else entries in
    let entries = if has_alpha then
      { ib_tag = Tags.extra_samples; ib_type = Tags.type_short; ib_count = 1;
        ib_value = Inline_short Tags.extra_unassoc_alpha } :: entries
    else entries in
    let num_entries = List.length entries in
    let ifd_offset = 8 in
    let ifd_size = 2 + num_entries * 12 + 4 in
    let ext_size = calc_external_size entries in
    let data_start = ifd_offset + ifd_size + ext_size in
    let strip_offsets, total_size = cumulative_offsets data_start strip_datas in
    let entries = List.map (fun e ->
      if e.ib_tag = Tags.strip_offsets then
        { e with ib_value = External_longs strip_offsets }
      else e
    ) entries in
    let buf = Bytes.make total_size '\000' in
    Bytes.set_uint8 buf 0 0x49;
    Bytes.set_uint8 buf 1 0x49;
    Byte_order.set_uint16 order buf 2 42;
    Byte_order.set_uint32 order buf 4 ifd_offset;
    let _ = write_ifd_and_data order buf ifd_offset entries in
    Array.iteri (fun i data ->
      Bytes.blit data 0 buf strip_offsets.(i) (Bytes.length data)
    ) strip_datas;
    buf

let write_bytes_with_options opts img =
  write_image_to_bytes opts img

let write_bytes img =
  write_image_to_bytes default_encode_options img

let write_with_options opts filename img =
  let buf = write_image_to_bytes opts img in
  let oc = open_out_bin filename in
  output_bytes oc buf;
  close_out oc

let write filename img =
  write_with_options default_encode_options filename img
