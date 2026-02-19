(** TIFF tag ID constants.

    Baseline TIFF tags from the TIFF 6.0 specification,
    plus GeoTIFF extension tags (34735-34737). *)

(* Baseline TIFF tags *)
let new_subfile_type = 254
let subfile_type = 255
let image_width = 256
let image_length = 257
let bits_per_sample = 258
let compression = 259
let photometric_interpretation = 262
let threshholding = 263
let cell_width = 264
let cell_length = 265
let fill_order = 266
let document_name = 269
let image_description = 270
let make = 271
let model = 272
let strip_offsets = 273
let orientation = 274
let samples_per_pixel = 277
let rows_per_strip = 278
let strip_byte_counts = 279
let min_sample_value = 280
let max_sample_value = 281
let x_resolution = 282
let y_resolution = 283
let planar_configuration = 284
let page_name = 285
let x_position = 286
let y_position = 287
let free_offsets = 288
let free_byte_counts = 289
let gray_response_unit = 290
let gray_response_curve = 291
let t4_options = 292
let t6_options = 293
let resolution_unit = 296
let page_number = 297
let transfer_function = 301
let software = 305
let date_time = 306
let artist = 315
let host_computer = 316
let predictor = 317
let white_point = 318
let primary_chromaticities = 319
let color_map = 320
let halftone_hints = 321
let tile_width = 322
let tile_length = 323
let tile_offsets = 324
let tile_byte_counts = 325
let sub_ifds = 330
let ink_set = 332
let ink_names = 333
let number_of_inks = 334
let dot_range = 336
let target_printer = 337
let extra_samples = 338
let sample_format = 339
let s_min_sample_value = 340
let s_max_sample_value = 341
let transfer_range = 342
let jpeg_proc = 512
let jpeg_interchange_format = 513
let jpeg_interchange_format_length = 514
let ycbcr_coefficients = 529
let ycbcr_sub_sampling = 530
let ycbcr_positioning = 531
let reference_black_white = 532
let copyright = 33432

(* ICC profile *)
let icc_profile = 34675

(* GeoTIFF tags *)
let geo_key_directory = 34735
let geo_double_params = 34736
let geo_ascii_params = 34737
let model_tiepoint = 33922
let model_pixel_scale = 33550
let model_transformation = 34264

(* GDAL metadata *)
let gdal_metadata = 42112
let gdal_nodata = 42113

(* TIFF field types *)
let type_byte = 1
let type_ascii = 2
let type_short = 3
let type_long = 4
let type_rational = 5
let type_sbyte = 6
let type_undefined = 7
let type_sshort = 8
let type_slong = 9
let type_srational = 10
let type_float = 11
let type_double = 12

let type_size = function
  | 1 -> 1   (* BYTE *)
  | 2 -> 1   (* ASCII *)
  | 3 -> 2   (* SHORT *)
  | 4 -> 4   (* LONG *)
  | 5 -> 8   (* RATIONAL *)
  | 6 -> 1   (* SBYTE *)
  | 7 -> 1   (* UNDEFINED *)
  | 8 -> 2   (* SSHORT *)
  | 9 -> 4   (* SLONG *)
  | 10 -> 8  (* SRATIONAL *)
  | 11 -> 4  (* FLOAT *)
  | 12 -> 8  (* DOUBLE *)
  | _ -> 1   (* unknown, treat as byte *)

(* Compression types *)
let compression_none = 1
let compression_ccitt_huffman = 2
let compression_ccitt_t4 = 3
let compression_ccitt_t6 = 4
let compression_lzw = 5
let compression_ojpeg = 6
let compression_jpeg = 7
let compression_deflate = 8
let compression_packbits = 32773
let compression_deflate_old = 32946

(* Photometric interpretation values *)
let photometric_white_is_zero = 0
let photometric_black_is_zero = 1
let photometric_rgb = 2
let photometric_palette = 3
let photometric_transparency_mask = 4
let photometric_cmyk = 5
let photometric_ycbcr = 6
let photometric_cielab = 8

(* Predictor values *)
let predictor_none = 1
let predictor_horizontal = 2
let predictor_floating_point = 3

(* PlanarConfiguration values *)
let planar_chunky = 1
let planar_planar = 2

(* Sample format values *)
let sample_format_uint = 1
let sample_format_int = 2
let sample_format_float = 3
let sample_format_undefined = 4

(* ExtraSamples values *)
let extra_unspecified = 0
let extra_assoc_alpha = 1
let extra_unassoc_alpha = 2

(* GeoKey IDs *)
let geo_gt_model_type = 1024
let geo_gt_raster_type = 1025
let geo_gt_citation = 1026
let geo_geographic_type = 2048
let geo_geog_citation = 2049
let geo_geog_geodetic_datum = 2050
let geo_geog_prime_meridian = 2051
let geo_geog_linear_units = 2052
let geo_geog_angular_units = 2054
let geo_geog_ellipsoid = 2056
let geo_geog_semi_major_axis = 2057
let geo_geog_semi_minor_axis = 2058
let geo_geog_inv_flattening = 2059
let geo_projected_cs_type = 3072
let geo_pcs_citation = 3073
let geo_projection = 3074
let geo_proj_coord_trans = 3075
let geo_proj_linear_units = 3076
let geo_proj_std_parallel1 = 3078
let geo_proj_std_parallel2 = 3079
let geo_proj_nat_origin_long = 3080
let geo_proj_nat_origin_lat = 3081
let geo_proj_false_easting = 3082
let geo_proj_false_northing = 3083
let geo_proj_center_long = 3088
let geo_proj_center_lat = 3089
let geo_proj_scale_at_nat_origin = 3092
let geo_vertical_cs_type = 4096
let geo_vertical_citation = 4097
let geo_vertical_datum = 4098
let geo_vertical_units = 4099
