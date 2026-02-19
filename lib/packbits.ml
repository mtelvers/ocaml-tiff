(** PackBits RLE decompression and compression (TIFF compression type 32773).

    Encoding scheme:
    - 0..127: copy next n+1 bytes literally
    - -127..-1: repeat next byte 1-n times (i.e., -n+1 copies)
    - -128: no-op *)

let decompress data =
  let len = Bytes.length data in
  let buf = Buffer.create (len * 2) in
  let rec copy_literal pos count =
    if count = 0 || pos >= len then pos
    else begin
      Buffer.add_char buf (Bytes.get data pos);
      copy_literal (pos + 1) (count - 1)
    end
  in
  let rec loop pos =
    if pos >= len then ()
    else
      let n = Bytes.get_uint8 data pos in
      let pos = pos + 1 in
      if n <= 127 then
        (* Copy n+1 literal bytes *)
        let pos = copy_literal pos (n + 1) in
        loop pos
      else if n = 128 then
        (* no-op *)
        loop pos
      else begin
        (* Repeat next byte (257 - n) times *)
        let count = 257 - n in
        if pos < len then begin
          let b = Bytes.get data pos in
          for _ = 1 to count do
            Buffer.add_char buf b
          done;
          loop (pos + 1)
        end
      end
  in
  loop 0;
  Buffer.contents buf |> Bytes.of_string

let compress data =
  let len = Bytes.length data in
  let buf = Buffer.create len in
  let rec find_run_end pos start b =
    if pos < len && pos - start < 128 && Bytes.get_uint8 data pos = b then
      find_run_end (pos + 1) start b
    else
      pos
  in
  let rec find_lit_end pos lit_start =
    if pos >= len || pos - lit_start >= 128 then pos
    else if pos + 2 < len &&
            Bytes.get_uint8 data pos = Bytes.get_uint8 data (pos + 1) &&
            Bytes.get_uint8 data pos = Bytes.get_uint8 data (pos + 2) then
      pos
    else
      find_lit_end (pos + 1) lit_start
  in
  let rec loop pos =
    if pos >= len then ()
    else
      let b = Bytes.get_uint8 data pos in
      let run_end = find_run_end (pos + 1) pos b in
      let run_len = run_end - pos in
      if run_len >= 3 then begin
        (* Encode as a run: header byte = 257 - run_len *)
        Buffer.add_char buf (Char.chr (257 - run_len));
        Buffer.add_char buf (Char.chr b);
        loop run_end
      end else begin
        (* Collect literal bytes *)
        let lit_end = find_lit_end pos pos in
        let lit_len = lit_end - pos in
        if lit_len > 0 then begin
          Buffer.add_char buf (Char.chr (lit_len - 1));
          for i = pos to lit_end - 1 do
            Buffer.add_char buf (Bytes.get data i)
          done
        end;
        loop lit_end
      end
  in
  loop 0;
  Buffer.contents buf |> Bytes.of_string
