(** IFD (Image File Directory) parsing.

    Parses the IFD chain from a TIFF file. Each IFD contains a set of
    tag entries describing one image (subfile). Handles inline vs offset
    values based on the TIFF type/count rules. *)

type entry = {
  tag : int;
  field_type : int;
  count : int;
  value_offset : int;  (* raw 4-byte value/offset field *)
}

type t = {
  entries : entry list;
  next_ifd_offset : int;
}

let parse_entry order buf off =
  let tag = Byte_order.get_uint16 order buf off in
  let field_type = Byte_order.get_uint16 order buf (off + 2) in
  let count = Byte_order.get_uint32 order buf (off + 4) in
  let value_offset = Byte_order.get_uint32 order buf (off + 8) in
  { tag; field_type; count; value_offset }

let parse order buf offset =
  let num_entries = Byte_order.get_uint16 order buf offset in
  let entries = List.init num_entries (fun i ->
    parse_entry order buf (offset + 2 + i * 12)
  ) in
  let next_off = offset + 2 + num_entries * 12 in
  let next_ifd_offset = Byte_order.get_uint32 order buf next_off in
  { entries; next_ifd_offset }

let parse_all order buf first_ifd_offset =
  let rec loop acc offset =
    if offset = 0 then List.rev acc
    else
      let ifd = parse order buf offset in
      loop (ifd :: acc) ifd.next_ifd_offset
  in
  loop [] first_ifd_offset

(** Check if value fits inline (within the 4-byte value/offset field). *)
let value_is_inline entry =
  let size = Tags.type_size entry.field_type in
  entry.count * size <= 4

(** Get the offset where the value data lives - either inline at the
    entry's position in the IFD, or at the pointed-to offset. *)
let value_data_offset entry ~ifd_offset ~entry_index =
  if value_is_inline entry then
    (* Inline: value is stored in the value/offset field itself,
       which is at offset ifd_offset + 2 + entry_index * 12 + 8 *)
    ifd_offset + 2 + entry_index * 12 + 8
  else
    entry.value_offset

(** Read a single integer value from an entry (BYTE, SHORT, or LONG).
    For SHORT/LONG that fits inline, reads from the value/offset field. *)
let get_int_value order buf entry =
  match entry.field_type with
  | 1 (* BYTE *) | 7 (* UNDEFINED *) ->
    Bytes.get_uint8 buf entry.value_offset
  | 3 (* SHORT *) ->
    if value_is_inline entry then
      (* For inline SHORT, the value is stored in the first 2 bytes
         of the value/offset field. We encoded value_offset as a uint32
         read, so the low 16 bits contain the first SHORT value. *)
      entry.value_offset land 0xFFFF
    else
      Byte_order.get_uint16 order buf entry.value_offset
  | 4 (* LONG *) ->
    entry.value_offset
  | _ -> entry.value_offset

(** Read a single integer value, accepting BYTE, SHORT, or LONG per TIFF spec. *)
let get_required_int order buf entry =
  get_int_value order buf entry

(** Read an array of integer values (BYTE, SHORT, or LONG). *)
let get_int_values order buf entry ~ifd_offset ~entry_index =
  let data_off = value_data_offset entry ~ifd_offset ~entry_index in
  Array.init entry.count (fun i ->
    match entry.field_type with
    | 1 (* BYTE *) | 7 (* UNDEFINED *) ->
      Bytes.get_uint8 buf (data_off + i)
    | 3 (* SHORT *) ->
      Byte_order.get_uint16 order buf (data_off + i * 2)
    | 4 (* LONG *) ->
      Byte_order.get_uint32 order buf (data_off + i * 4)
    | _ ->
      Byte_order.get_uint32 order buf (data_off + i * 4)
  )

(** Read an array of SHORT values. *)
let get_short_values order buf entry ~ifd_offset ~entry_index =
  let data_off = value_data_offset entry ~ifd_offset ~entry_index in
  Array.init entry.count (fun i ->
    Byte_order.get_uint16 order buf (data_off + i * 2)
  )

(** Read an array of LONG values. *)
let get_long_values order buf entry ~ifd_offset ~entry_index =
  let data_off = value_data_offset entry ~ifd_offset ~entry_index in
  Array.init entry.count (fun i ->
    Byte_order.get_uint32 order buf (data_off + i * 4)
  )

(** Read an array of DOUBLE values. *)
let get_double_values order buf entry ~ifd_offset ~entry_index =
  let data_off = value_data_offset entry ~ifd_offset ~entry_index in
  Array.init entry.count (fun i ->
    Byte_order.get_double order buf (data_off + i * 8)
  )

(** Read an ASCII string value. *)
let get_ascii_value _order buf entry ~ifd_offset ~entry_index =
  let data_off = value_data_offset entry ~ifd_offset ~entry_index in
  (* Count includes the NUL terminator *)
  let len = if entry.count > 0 then entry.count - 1 else 0 in
  Bytes.sub_string buf data_off len

(** Read raw bytes for an entry's value. *)
let get_bytes buf entry ~ifd_offset ~entry_index =
  let data_off = value_data_offset entry ~ifd_offset ~entry_index in
  let size = Tags.type_size entry.field_type in
  Bytes.sub buf data_off (entry.count * size)

(** Find an entry by tag in an IFD. *)
let find_entry ifd tag =
  List.find_opt (fun e -> e.tag = tag) ifd.entries

(** Find entry index by tag. *)
let find_entry_index ifd tag =
  let rec loop i = function
    | [] -> None
    | e :: _ when e.tag = tag -> Some (e, i)
    | _ :: rest -> loop (i + 1) rest
  in
  loop 0 ifd.entries
