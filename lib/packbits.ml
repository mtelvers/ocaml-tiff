(** PackBits RLE decompression and compression (TIFF compression type 32773).

    Encoding scheme:
    - 0..127: copy next n+1 bytes literally
    - -127..-1: repeat next byte 1-n times (i.e., -n+1 copies)
    - -128: no-op *)

let decompress data =
  let len = Bytes.length data in
  let buf = Buffer.create (len * 2) in
  let pos = ref 0 in
  while !pos < len do
    let n = Bytes.get_uint8 data !pos in
    pos := !pos + 1;
    if n <= 127 then begin
      (* Copy n+1 literal bytes *)
      let count = n + 1 in
      for _ = 1 to count do
        if !pos < len then begin
          Buffer.add_char buf (Bytes.get data !pos);
          pos := !pos + 1
        end
      done
    end else if n = 128 then
      () (* no-op *)
    else begin
      (* Repeat next byte (257 - n) times *)
      let count = 257 - n in
      if !pos < len then begin
        let b = Bytes.get data !pos in
        pos := !pos + 1;
        for _ = 1 to count do
          Buffer.add_char buf b
        done
      end
    end
  done;
  Buffer.contents buf |> Bytes.of_string

let compress data =
  let len = Bytes.length data in
  let buf = Buffer.create len in
  let pos = ref 0 in
  while !pos < len do
    (* Look for a run of identical bytes *)
    let start = !pos in
    let b = Bytes.get_uint8 data start in
    let run_end = ref (start + 1) in
    while !run_end < len && !run_end - start < 128 &&
          Bytes.get_uint8 data !run_end = b do
      run_end := !run_end + 1
    done;
    let run_len = !run_end - start in
    if run_len >= 3 then begin
      (* Encode as a run: header byte = 257 - run_len *)
      Buffer.add_char buf (Char.chr (257 - run_len));
      Buffer.add_char buf (Char.chr b);
      pos := !run_end
    end else begin
      (* Collect literal bytes *)
      let lit_start = start in
      let lit_end = ref start in
      let stop = ref false in
      while !lit_end < len && !lit_end - lit_start < 128 && not !stop do
        if !lit_end + 2 < len &&
           Bytes.get_uint8 data !lit_end = Bytes.get_uint8 data (!lit_end + 1) &&
           Bytes.get_uint8 data !lit_end = Bytes.get_uint8 data (!lit_end + 2) then
          stop := true
        else
          lit_end := !lit_end + 1
      done;
      let lit_len = !lit_end - lit_start in
      if lit_len > 0 then begin
        Buffer.add_char buf (Char.chr (lit_len - 1));
        for i = lit_start to !lit_end - 1 do
          Buffer.add_char buf (Bytes.get data i)
        done
      end;
      pos := !lit_end
    end
  done;
  Buffer.contents buf |> Bytes.of_string
