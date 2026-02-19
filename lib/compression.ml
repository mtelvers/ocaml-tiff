(** Compression dispatcher for TIFF.

    Routes decompression/compression to the appropriate module
    based on the TIFF compression tag value. *)

let decompress ~compression data =
  match compression with
  | 1 -> data  (* No compression *)
  | 5 -> Lzw.decompress data
  | 8 | 32946 -> Deflate.inflate data
  | 32773 -> Packbits.decompress data
  | n -> failwith (Printf.sprintf "TIFF: unsupported compression type %d" n)

type compression_option =
  | Compress_none
  | Compress_packbits
  | Compress_lzw
  | Compress_deflate

let compress ~compression data =
  match compression with
  | Compress_none -> data
  | Compress_packbits -> Packbits.compress data
  | Compress_lzw -> Lzw.compress data
  | Compress_deflate -> Deflate.deflate data

let compression_tag = function
  | Compress_none -> Tags.compression_none
  | Compress_packbits -> Tags.compression_packbits
  | Compress_lzw -> Tags.compression_lzw
  | Compress_deflate -> Tags.compression_deflate
