(** GeoKey directory parsing for GeoTIFF.

    The GeoKeyDirectoryTag (34735) contains a SHORT array structured as:
    - Header: KeyDirectoryVersion, KeyRevision, MinorRevision, NumberOfKeys
    - Key entries: KeyID, TIFFTagLocation, Count, ValueOffset

    If TIFFTagLocation = 0, the value is the SHORT in ValueOffset.
    If TIFFTagLocation = 34736, the value is DOUBLE(s) from GeoDoubleParamsTag.
    If TIFFTagLocation = 34737, the value is ASCII from GeoAsciiParamsTag. *)

type geo_key = {
  key_id : int;
  tiff_tag_location : int;
  count : int;
  value_offset : int;
}

type t = {
  version : int;
  key_revision : int;
  minor_revision : int;
  keys : geo_key list;
}

let parse_directory shorts =
  let n = Array.length shorts in
  if n < 4 then None
  else
    let version = shorts.(0) in
    let key_revision = shorts.(1) in
    let minor_revision = shorts.(2) in
    let num_keys = shorts.(3) in
    if n < 4 + num_keys * 4 then None
    else
      let keys = List.init num_keys (fun i ->
        let off = 4 + i * 4 in
        { key_id = shorts.(off);
          tiff_tag_location = shorts.(off + 1);
          count = shorts.(off + 2);
          value_offset = shorts.(off + 3);
        }
      ) in
      Some { version; key_revision; minor_revision; keys }

let find_key t key_id =
  List.find_opt (fun k -> k.key_id = key_id) t.keys

let get_short_value t key_id =
  match find_key t key_id with
  | Some k when k.tiff_tag_location = 0 -> Some k.value_offset
  | _ -> None

let get_double_value t key_id ~double_params =
  match find_key t key_id with
  | Some k when k.tiff_tag_location = Tags.geo_double_params ->
    if k.value_offset < Array.length double_params then
      Some double_params.(k.value_offset)
    else
      None
  | _ -> None

let get_double_values t key_id ~double_params =
  match find_key t key_id with
  | Some k when k.tiff_tag_location = Tags.geo_double_params ->
    if k.value_offset + k.count <= Array.length double_params then
      Some (Array.sub double_params k.value_offset k.count)
    else
      None
  | _ -> None

let get_ascii_value t key_id ~ascii_params =
  match find_key t key_id with
  | Some k when k.tiff_tag_location = Tags.geo_ascii_params ->
    let len = String.length ascii_params in
    if k.value_offset + k.count <= len then begin
      let s = String.sub ascii_params k.value_offset k.count in
      (* Strip trailing pipe character and NUL *)
      let s = if String.length s > 0 &&
                 (s.[String.length s - 1] = '|' || s.[String.length s - 1] = '\000')
              then String.sub s 0 (String.length s - 1)
              else s in
      Some s
    end else
      None
  | _ -> None

let model_type t = get_short_value t Tags.geo_gt_model_type
let raster_type t = get_short_value t Tags.geo_gt_raster_type
let projected_cs_type t = get_short_value t Tags.geo_projected_cs_type
let geographic_type t = get_short_value t Tags.geo_geographic_type
