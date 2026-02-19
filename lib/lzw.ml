(** LZW decompression and compression for TIFF.

    TIFF LZW uses MSB-first bit packing (opposite of GIF).
    Codes start at 9 bits, grow to 12 bits max.
    Clear code = 256, EOI code = 257, first data code = 258. *)

let clear_code = 256
let eoi_code = 257
let first_code = 258
let max_code_size = 12
let max_table_size = 1 lsl max_code_size  (* 4096 *)

(* ---- MSB-first bit reader ---- *)

type bit_reader = {
  data : bytes;
  data_len : int;
  mutable byte_pos : int;
  mutable bit_pos : int;  (* bits consumed in current byte, 0 = fresh *)
}

let create_reader data =
  { data; data_len = Bytes.length data; byte_pos = 0; bit_pos = 0 }

let rec read_code reader nbits acc =
  if nbits = 0 then acc
  else begin
    if reader.byte_pos >= reader.data_len then
      failwith "TIFF: unexpected end of LZW data";
    let cur_byte = Bytes.get_uint8 reader.data reader.byte_pos in
    let bits_avail = 8 - reader.bit_pos in
    let bits_to_read = min bits_avail nbits in
    let shift = bits_avail - bits_to_read in
    let bits = (cur_byte lsr shift) land ((1 lsl bits_to_read) - 1) in
    reader.bit_pos <- reader.bit_pos + bits_to_read;
    if reader.bit_pos >= 8 then begin
      reader.bit_pos <- 0;
      reader.byte_pos <- reader.byte_pos + 1
    end;
    read_code reader (nbits - bits_to_read) ((acc lsl bits_to_read) lor bits)
  end

(* ---- MSB-first bit writer ---- *)

type bit_writer = {
  buf : Buffer.t;
  mutable wbyte : int;
  mutable wbit_pos : int;  (* bits written in current byte, 0-7 *)
}

let create_writer () =
  { buf = Buffer.create 4096; wbyte = 0; wbit_pos = 0 }

let rec write_code writer code nbits =
  if nbits > 0 then begin
    let bit = (code lsr (nbits - 1)) land 1 in
    writer.wbyte <- (writer.wbyte lsl 1) lor bit;
    writer.wbit_pos <- writer.wbit_pos + 1;
    if writer.wbit_pos = 8 then begin
      Buffer.add_char writer.buf (Char.chr writer.wbyte);
      writer.wbyte <- 0;
      writer.wbit_pos <- 0
    end;
    write_code writer code (nbits - 1)
  end

let flush_writer writer =
  if writer.wbit_pos > 0 then
    Buffer.add_char writer.buf (Char.chr (writer.wbyte lsl (8 - writer.wbit_pos)))

(* ---- Decompression ---- *)

type table_entry = {
  prefix : int;  (* -1 for single-byte entries *)
  suffix : int;
  length : int;
}

(* Walk the prefix chain to reconstruct the string for a code *)
let emit_string table output code =
  let entry = table.(code) in
  let tmp = Bytes.create entry.length in
  let rec walk c i =
    Bytes.set_uint8 tmp i table.(c).suffix;
    if table.(c).prefix >= 0 then walk table.(c).prefix (i - 1)
  in
  walk code (entry.length - 1);
  Buffer.add_bytes output tmp;
  Bytes.get_uint8 tmp 0

let init_decode_table table =
  Array.iteri (fun i _ ->
    table.(i) <- { prefix = -1; suffix = (if i < 256 then i else 0); length = 1 }
  ) table

(* Decoder state threaded through recursive calls *)
type decode_state = { next_code : int; code_size : int }

let bump_code_size state =
  (* Decoder lags encoder by 1 table entry; bump one step early to stay in sync *)
  if state.next_code >= (1 lsl state.code_size) - 1
     && state.code_size < max_code_size then
    { state with code_size = state.code_size + 1 }
  else
    state

let add_table_entry table state ~old_code ~first_byte =
  if state.next_code < max_table_size then begin
    table.(state.next_code) <- {
      prefix = old_code;
      suffix = first_byte;
      length = table.(old_code).length + 1;
    };
    bump_code_size { state with next_code = state.next_code + 1 }
  end else
    state

let decompress data =
  let reader = create_reader data in
  let output = Buffer.create (Bytes.length data * 3) in
  let table = Array.make max_table_size { prefix = -1; suffix = 0; length = 1 } in
  init_decode_table table;
  let initial_state = { next_code = first_code; code_size = 9 } in
  (* After a clear code: read first data code, then enter main loop *)
  let rec after_clear state =
    let code = read_code reader state.code_size 0 in
    if code = eoi_code then ()
    else begin
      let _ = emit_string table output code in
      decode_loop state code
    end
  (* Main decode loop *)
  and decode_loop state old_code =
    let code = read_code reader state.code_size 0 in
    if code = eoi_code then ()
    else if code = clear_code then begin
      init_decode_table table;
      after_clear initial_state
    end else begin
      let first_byte =
        if code < state.next_code then
          emit_string table output code
        else if code = state.next_code then begin
          (* KwKwK case: string = old_string + first_byte_of_old_string *)
          let fb = emit_string table output old_code in
          Buffer.add_char output (Char.chr fb);
          fb
        end else
          failwith "TIFF: invalid LZW code"
      in
      let state = add_table_entry table state ~old_code ~first_byte in
      decode_loop state code
    end
  in
  let first = read_code reader initial_state.code_size 0 in
  if first <> clear_code then failwith "TIFF: LZW data must begin with clear code";
  after_clear initial_state;
  Buffer.contents output |> Bytes.of_string

(* ---- Compression ---- *)

(* Encoder state threaded through recursive calls *)
type encode_state = { enc_next_code : int; enc_code_size : int }

let enc_bump_code_size state =
  if state.enc_next_code > (1 lsl state.enc_code_size) - 1
     && state.enc_code_size < max_code_size then
    { state with enc_code_size = state.enc_code_size + 1 }
  else
    state

let initial_encode_state = { enc_next_code = first_code; enc_code_size = 9 }

let compress data =
  let len = Bytes.length data in
  let writer = create_writer () in
  let htab : (int * int, int) Hashtbl.t = Hashtbl.create 4096 in
  let reset_table () =
    Hashtbl.clear htab;
    initial_encode_state
  in
  let state = reset_table () in
  write_code writer clear_code state.enc_code_size;
  if len = 0 then begin
    write_code writer eoi_code state.enc_code_size;
    flush_writer writer;
    Buffer.contents writer.buf |> Bytes.of_string
  end else begin
    let rec encode_loop state w i =
      if i >= len then begin
        write_code writer w state.enc_code_size;
        write_code writer eoi_code state.enc_code_size;
      end else
        let k = Bytes.get_uint8 data i in
        match Hashtbl.find_opt htab (w, k) with
        | Some code ->
          encode_loop state code (i + 1)
        | None ->
          write_code writer w state.enc_code_size;
          if state.enc_next_code < max_table_size then begin
            Hashtbl.replace htab (w, k) state.enc_next_code;
            let state = enc_bump_code_size
              { state with enc_next_code = state.enc_next_code + 1 } in
            encode_loop state k (i + 1)
          end else begin
            write_code writer clear_code state.enc_code_size;
            let state = reset_table () in
            encode_loop state k (i + 1)
          end
    in
    encode_loop state (Bytes.get_uint8 data 0) 1;
    flush_writer writer;
    Buffer.contents writer.buf |> Bytes.of_string
  end
