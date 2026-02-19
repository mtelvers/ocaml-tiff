(** Endian-aware binary reading for TIFF files.

    TIFF files begin with either "II" (little-endian) or "MM" (big-endian).
    All multi-byte values in the file use the specified byte order. *)

type t = LE | BE

let detect buf =
  if Bytes.length buf < 2 then failwith "TIFF: data too short for byte order";
  let b0 = Bytes.get_uint8 buf 0 in
  let b1 = Bytes.get_uint8 buf 1 in
  match (b0, b1) with
  | (0x49, 0x49) -> LE
  | (0x4D, 0x4D) -> BE
  | _ -> failwith "TIFF: invalid byte order marker"

let get_uint16 order buf off =
  match order with
  | LE -> Bytes.get_uint16_le buf off
  | BE -> Bytes.get_uint16_be buf off

let get_int32 order buf off =
  match order with
  | LE -> Bytes.get_int32_le buf off
  | BE -> Bytes.get_int32_be buf off

let get_uint32 order buf off =
  Int32.to_int (get_int32 order buf off) land 0xFFFFFFFF

let get_int64 order buf off =
  match order with
  | LE -> Bytes.get_int64_le buf off
  | BE -> Bytes.get_int64_be buf off

let get_double order buf off =
  Int64.float_of_bits (get_int64 order buf off)

let get_float order buf off =
  Int32.float_of_bits (get_int32 order buf off)

let set_uint16 order buf off v =
  match order with
  | LE -> Bytes.set_uint16_le buf off v
  | BE -> Bytes.set_uint16_be buf off v

let set_int32 order buf off v =
  match order with
  | LE -> Bytes.set_int32_le buf off v
  | BE -> Bytes.set_int32_be buf off v

let set_uint32 order buf off v =
  set_int32 order buf off (Int32.of_int v)

let set_int64 order buf off v =
  match order with
  | LE -> Bytes.set_int64_le buf off v
  | BE -> Bytes.set_int64_be buf off v

let set_double order buf off v =
  set_int64 order buf off (Int64.bits_of_float v)

let get_int16 order buf off =
  match order with LE -> Bytes.get_int16_le buf off | BE -> Bytes.get_int16_be buf off

let set_int16 order buf off v =
  match order with LE -> Bytes.set_int16_le buf off v | BE -> Bytes.set_int16_be buf off v
