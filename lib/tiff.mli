(** Pure OCaml TIFF/GeoTIFF library for reading and writing TIFF files.

    Supported features:
    - Strip-based and tile-based images
    - Compression: None, PackBits, LZW, Deflate
    - Photometric: WhiteIsZero, BlackIsZero, RGB, Palette, CMYK, YCbCr
    - Bit depths: 1, 2, 4, 8, 16 (normalized to 8-bit output)
    - Horizontal differencing predictor
    - Multi-IFD files (COG overviews)
    - GeoTIFF metadata (GeoKeys, coordinate transformations)
    - ICC color profiles
    - Native raster data access (uint8, uint16, int16, int32, uint32, float32, float64) *)

(** {1 Internal Modules}
    These modules are exposed for advanced use cases. *)

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

(** {1 Types} *)

(** Pixel format for image data *)
type pixel_format =
  | RGB24   (** 3 bytes per pixel: R, G, B *)
  | RGBA32  (** 4 bytes per pixel: R, G, B, A *)

type pixel_data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Pixel data stored as RGB24 or RGBA32 format in a bigarray. *)

type image = {
  width : int;
  height : int;
  pixels : pixel_data;
  pixel_format : pixel_format;
  icc_profile : Icc_tiff.t option;
}
(** TIFF image with pixel data and optional ICC color profile. *)

(** {1 GeoTIFF Types} *)

type geo_metadata = {
  geokeys : Geokeys.t option;
  transform : Geotransform.t;
  double_params : float list;
  ascii_params : string;
}
(** GeoTIFF metadata including GeoKeys, coordinate transformation,
    and parameter arrays. *)

type geo_image = {
  image : image;
  geo : geo_metadata;
}
(** A TIFF image with associated GeoTIFF metadata. *)

(** {1 Raster Types} *)

(** Native sample format of the TIFF data. *)
type sample_format =
  | Uint8    (** Unsigned 8-bit integer *)
  | Uint16   (** Unsigned 16-bit integer *)
  | Int16    (** Signed 16-bit integer *)
  | Int32    (** Signed 32-bit integer *)
  | Uint32   (** Unsigned 32-bit integer (stored as int32) *)
  | Float32  (** IEEE 32-bit float *)
  | Float64  (** IEEE 64-bit float *)

(** Raster data in its native sample type. *)
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
(** Raster with native sample types, preserving full precision.
    Data is band-interleaved (pixel-major): for a 3-band image,
    samples are ordered [b0 b1 b2 b0 b1 b2 ...]. *)

type geo_raster = {
  raster : raster;
  geo : geo_metadata;
}
(** A raster with associated GeoTIFF metadata. *)

(** {1 IFD Info} *)

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
(** Metadata about a single IFD without decoding pixel data.
    Useful for inspecting COG overviews. *)

(** {1 Encoding Options} *)

type compression_option =
  | Compress_none      (** No compression *)
  | Compress_packbits  (** PackBits RLE compression *)
  | Compress_lzw       (** LZW compression *)
  | Compress_deflate   (** Deflate/zlib compression *)

type encode_options = {
  compression : compression_option;
  tile_size : int option;  (** [None] for strips, [Some n] for n×n tiles *)
  predictor : bool;        (** Horizontal differencing for LZW/Deflate *)
}

val default_encode_options : encode_options
(** Default encoding options: LZW compression, strips, no predictor. *)

(** {1 Reading TIFF} *)

val read : string -> image
(** [read filename] reads a TIFF image from the given file path.
    All color types are converted to RGB24 or RGBA32 (if alpha is present).
    @raise Failure if the file cannot be read or is not a valid TIFF. *)

val read_bytes : bytes -> image
(** [read_bytes data] decodes a TIFF image from bytes in memory.
    @raise Failure if the data is not a valid TIFF. *)

val read_ifd : string -> int -> image
(** [read_ifd filename n] reads the nth IFD from a TIFF file.
    Useful for accessing COG overviews.
    @raise Failure if n is out of range. *)

(** {1 Reading GeoTIFF} *)

val read_geo : string -> geo_image
(** [read_geo filename] reads a GeoTIFF image with metadata.
    @raise Failure if the file cannot be read or is not a valid TIFF. *)

val read_geo_bytes : bytes -> geo_image
(** [read_geo_bytes data] decodes a GeoTIFF image from bytes in memory. *)

val read_geo_ifd : string -> int -> geo_image
(** [read_geo_ifd filename n] reads the nth IFD as a GeoTIFF image. *)

(** {1 Reading Raster Data}

    These functions preserve native sample types (uint16, float32, etc.)
    instead of normalizing to 8-bit RGB. Use these for scientific data,
    DEMs, satellite imagery, and other cases where precision matters. *)

val read_raster : string -> raster
(** [read_raster filename] reads a TIFF as a native-type raster. *)

val read_raster_bytes : bytes -> raster
(** [read_raster_bytes data] decodes a TIFF raster from bytes in memory. *)

val read_raster_ifd : string -> int -> raster
(** [read_raster_ifd filename n] reads the nth IFD as a native-type raster. *)

val read_geo_raster : string -> geo_raster
(** [read_geo_raster filename] reads a GeoTIFF as a native-type raster
    with metadata. *)

val read_geo_raster_bytes : bytes -> geo_raster
(** [read_geo_raster_bytes data] decodes a GeoTIFF raster from bytes. *)

val read_geo_raster_ifd : string -> int -> geo_raster
(** [read_geo_raster_ifd filename n] reads the nth IFD as a geo raster. *)

(** {1 Writing TIFF} *)

val write : string -> image -> unit
(** [write filename image] writes an image to the given file path as TIFF
    using default encoding options.
    @raise Failure if the file cannot be written. *)

val write_bytes : image -> bytes
(** [write_bytes image] encodes an image to TIFF bytes in memory using
    default encoding options. *)

val write_with_options : encode_options -> string -> image -> unit
(** [write_with_options options filename image] writes an image to the given
    file path with the specified encoding options.
    @raise Failure if the file cannot be written. *)

val write_bytes_with_options : encode_options -> image -> bytes
(** [write_bytes_with_options options image] encodes an image to TIFF bytes
    with the specified encoding options. *)

(** {1 Image Creation} *)

val create_image : int -> int -> pixel_data -> image
(** [create_image width height pixels] creates a new image from raw RGB24
    pixel data. The [pixels] bigarray must have length [width * height * 3]. *)

val create_rgba_image : int -> int -> pixel_data -> image
(** [create_rgba_image width height pixels] creates a new RGBA image from raw
    RGBA32 pixel data. The [pixels] bigarray must have length
    [width * height * 4]. *)

val create_image_with_icc : int -> int -> pixel_data -> Icc_tiff.t -> image
(** [create_image_with_icc width height pixels icc] creates a new image with
    ICC color profile. *)

(** {1 Pixel Access} *)

val get_pixel : image -> int -> int -> int * int * int
(** [get_pixel image x y] returns the (r, g, b) values at position (x, y).
    For RGBA images, the alpha channel is ignored. *)

val get_pixel_rgba : image -> int -> int -> int * int * int * int
(** [get_pixel_rgba image x y] returns the (r, g, b, a) values at position
    (x, y). For RGB images, alpha is always 255. *)

val set_pixel : image -> int -> int -> int -> int -> int -> unit
(** [set_pixel image x y r g b] sets the pixel at position (x, y) to
    (r, g, b). For RGBA images, alpha is set to 255. *)

val set_pixel_rgba : image -> int -> int -> int -> int -> int -> int -> unit
(** [set_pixel_rgba image x y r g b a] sets the RGBA pixel at position (x, y).
    Only valid for RGBA32 images. *)

(** {1 GeoTIFF Utilities} *)

val raster_to_model : geo_image -> float -> float -> (float * float) option
(** [raster_to_model geo_img x y] converts raster coordinates to model
    (geographic/projected) coordinates. *)

val model_to_raster : geo_image -> float -> float -> (float * float) option
(** [model_to_raster geo_img x y] converts model coordinates to raster
    coordinates. *)

val bounding_box : geo_image -> (float * float * float * float) option
(** [bounding_box geo_img] returns the geographic bounding box as
    [(min_x, min_y, max_x, max_y)]. *)

val epsg_code : geo_image -> int option
(** [epsg_code geo_img] returns the EPSG code from the GeoTIFF metadata,
    checking ProjectedCSType first, then GeographicType. *)

(** {1 File Inspection} *)

val ifd_count : string -> int
(** [ifd_count filename] returns the number of IFDs in a TIFF file. *)

val ifd_info : string -> ifd_info list
(** [ifd_info filename] returns metadata for each IFD without decoding
    pixel data. Useful for COG introspection. *)
