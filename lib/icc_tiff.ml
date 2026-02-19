(** ICC color profile handling for TIFF (tag 34675).

    Unlike PNG where ICC profiles are compressed in the iCCP chunk,
    TIFF stores ICC profiles as raw bytes in tag 34675. *)

type t = { data : bytes }

let empty = { data = Bytes.empty }

let from_bytes data = { data }

let to_bytes t = t.data

let is_empty t = Bytes.length t.data = 0
