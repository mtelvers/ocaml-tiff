(** Pure OCaml deflate/inflate implementation with zlib wrapper.
    Copied from ocaml-png for zero-dependency TIFF support. *)

(* ---- Adler32 checksum ---- *)

let adler32_update a1 a2 buf off len =
  let nmax = 5552 in
  let s1 = ref a1 in
  let s2 = ref a2 in
  let remaining = ref len in
  let pos = ref off in
  while !remaining > 0 do
    let n = min !remaining nmax in
    for i = !pos to !pos + n - 1 do
      s1 := !s1 + Bytes.get_uint8 buf i;
      s2 := !s2 + !s1
    done;
    s1 := !s1 mod 65521;
    s2 := !s2 mod 65521;
    pos := !pos + n;
    remaining := !remaining - n
  done;
  (!s1, !s2)

let adler32 buf off len =
  let s1, s2 = adler32_update 1 0 buf off len in
  Int32.logor (Int32.shift_left (Int32.of_int s2) 16) (Int32.of_int s1)

(* ---- Bit manipulation helpers ---- *)

let reverse_bits v n =
  let rec loop acc v i =
    if i = 0 then acc
    else loop ((acc lsl 1) lor (v land 1)) (v lsr 1) (i - 1)
  in
  loop 0 v n

(* ---- Bit reader for inflate ---- *)

type bit_reader = {
  data : bytes;
  data_len : int;
  mutable pos : int;
  mutable bit_buf : int;
  mutable bit_count : int;
}

let create_reader data =
  { data; data_len = Bytes.length data; pos = 0; bit_buf = 0; bit_count = 0 }

let read_byte r =
  if r.pos >= r.data_len then failwith "TIFF: unexpected end of compressed data";
  let b = Bytes.get_uint8 r.data r.pos in
  r.pos <- r.pos + 1;
  b

let read_bits r n =
  while r.bit_count < n do
    let b = read_byte r in
    r.bit_buf <- r.bit_buf lor (b lsl r.bit_count);
    r.bit_count <- r.bit_count + 8
  done;
  let result = r.bit_buf land ((1 lsl n) - 1) in
  r.bit_buf <- r.bit_buf lsr n;
  r.bit_count <- r.bit_count - n;
  result

let drop_to_byte_boundary r =
  r.bit_buf <- 0;
  r.bit_count <- 0

(* ---- Huffman table with fast lookup ---- *)

let lk_bits = 9
let lk_mask = (1 lsl lk_bits) - 1

type huffman_table = {
  counts : int array;
  symbols : int array;
  lookup : int array;
}

let build_huffman_table lengths n_symbols =
  let max_bits = 15 in
  let counts = Array.make (max_bits + 1) 0 in
  for i = 0 to n_symbols - 1 do
    if lengths.(i) > 0 then
      counts.(lengths.(i)) <- counts.(lengths.(i)) + 1
  done;
  let offsets = Array.make (max_bits + 1) 0 in
  let total =
    let rec loop i acc =
      if i > max_bits then acc
      else begin
        offsets.(i) <- acc;
        loop (i + 1) (acc + counts.(i))
      end
    in
    loop 1 0
  in
  let symbols = Array.make total 0 in
  for i = 0 to n_symbols - 1 do
    if lengths.(i) > 0 then begin
      symbols.(offsets.(lengths.(i))) <- i;
      offsets.(lengths.(i)) <- offsets.(lengths.(i)) + 1
    end
  done;
  let lookup = Array.make (1 lsl lk_bits) 0 in
  let next_code = Array.make (max_bits + 1) 0 in
  let () =
    let rec loop bits code =
      if bits > max_bits then ()
      else begin
        let code = (code + counts.(bits - 1)) lsl 1 in
        next_code.(bits) <- code;
        loop (bits + 1) code
      end
    in
    loop 1 0
  in
  for i = 0 to n_symbols - 1 do
    let len = lengths.(i) in
    if len > 0 then begin
      let c = next_code.(len) in
      next_code.(len) <- c + 1;
      if len <= lk_bits then begin
        let rev_c = reverse_bits c len in
        let fill = 1 lsl (lk_bits - len) in
        for j = 0 to fill - 1 do
          lookup.((j lsl len) lor rev_c) <- (len lsl 16) lor i
        done
      end
    end
  done;
  { counts; symbols; lookup }

let decode_symbol_slow r ht =
  let rec loop code first index len =
    if len > 15 then failwith "TIFF: invalid Huffman code in compressed data"
    else
      let bit = read_bits r 1 in
      let code = (code lsl 1) lor bit in
      let count = ht.counts.(len) in
      if code - first < count then
        ht.symbols.(index + code - first)
      else
        loop code ((first + count) lsl 1) (index + count) (len + 1)
  in
  loop 0 0 0 1

let decode_symbol r ht =
  while r.bit_count < lk_bits && r.pos < r.data_len do
    r.bit_buf <- r.bit_buf lor (Bytes.get_uint8 r.data r.pos lsl r.bit_count);
    r.pos <- r.pos + 1;
    r.bit_count <- r.bit_count + 8
  done;
  if r.bit_count >= lk_bits then
    let entry = ht.lookup.(r.bit_buf land lk_mask) in
    if entry <> 0 then begin
      let len = entry lsr 16 in
      r.bit_buf <- r.bit_buf lsr len;
      r.bit_count <- r.bit_count - len;
      entry land 0xFFFF
    end else
      decode_symbol_slow r ht
  else
    decode_symbol_slow r ht

(* ---- Fixed Huffman tables (RFC 1951 section 3.2.6) ---- *)

let fixed_litlen_table =
  let lengths = Array.make 288 0 in
  for i = 0 to 143 do lengths.(i) <- 8 done;
  for i = 144 to 255 do lengths.(i) <- 9 done;
  for i = 256 to 279 do lengths.(i) <- 7 done;
  for i = 280 to 287 do lengths.(i) <- 8 done;
  build_huffman_table lengths 288

let fixed_dist_table =
  let lengths = Array.make 32 0 in
  Array.fill lengths 0 32 5;
  build_huffman_table lengths 32

(* ---- Length and distance extra bits tables ---- *)

let length_base = [|
  3; 4; 5; 6; 7; 8; 9; 10; 11; 13; 15; 17; 19; 23; 27; 31;
  35; 43; 51; 59; 67; 83; 99; 115; 131; 163; 195; 227; 258
|]

let length_extra = [|
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2;
  3; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5; 0
|]

let dist_base = [|
  1; 2; 3; 4; 5; 7; 9; 13; 17; 25; 33; 49; 65; 97; 129; 193;
  257; 385; 513; 769; 1025; 1537; 2049; 3073; 4097; 6145; 8193; 12289; 16385; 24577
|]

let dist_extra = [|
  0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6;
  7; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 13; 13
|]

let codelen_order = [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

(* ---- Inflate ---- *)

let window_mask = 0x7FFF

let inflate_raw data =
  let r = create_reader data in
  let out_buf = Buffer.create (Bytes.length data * 3) in
  let window = Bytes.create 32768 in
  let wpos = ref 0 in
  let emit_byte b =
    Buffer.add_char out_buf (Char.chr b);
    Bytes.set_uint8 window (!wpos land window_mask) b;
    wpos := !wpos + 1
  in
  let decode_lz litlen_table dist_table =
    let done_ = ref false in
    while not !done_ do
      let sym = decode_symbol r litlen_table in
      if sym < 256 then
        emit_byte sym
      else if sym = 256 then
        done_ := true
      else begin
        let len_idx = sym - 257 in
        let length = length_base.(len_idx) + read_bits r length_extra.(len_idx) in
        let dist_sym = decode_symbol r dist_table in
        let distance = dist_base.(dist_sym) + read_bits r dist_extra.(dist_sym) in
        for _ = 1 to length do
          let src = (!wpos - distance) land window_mask in
          emit_byte (Bytes.get_uint8 window src)
        done
      end
    done
  in
  let bfinal = ref false in
  while not !bfinal do
    let final = read_bits r 1 in
    bfinal := final = 1;
    let btype = read_bits r 2 in
    match btype with
    | 0 ->
      drop_to_byte_boundary r;
      let len = read_bits r 16 in
      let _nlen = read_bits r 16 in
      for _ = 1 to len do
        emit_byte (read_byte r)
      done
    | 1 ->
      decode_lz fixed_litlen_table fixed_dist_table
    | 2 ->
      let hlit = read_bits r 5 + 257 in
      let hdist = read_bits r 5 + 1 in
      let hclen = read_bits r 4 + 4 in
      let cl_lengths = Array.make 19 0 in
      for i = 0 to hclen - 1 do
        cl_lengths.(codelen_order.(i)) <- read_bits r 3
      done;
      let cl_table = build_huffman_table cl_lengths 19 in
      let all_lengths = Array.make (hlit + hdist) 0 in
      let i = ref 0 in
      while !i < hlit + hdist do
        let sym = decode_symbol r cl_table in
        if sym < 16 then begin
          all_lengths.(!i) <- sym;
          i := !i + 1
        end else if sym = 16 then begin
          let repeat = read_bits r 2 + 3 in
          let prev = if !i > 0 then all_lengths.(!i - 1) else 0 in
          for _ = 1 to repeat do
            all_lengths.(!i) <- prev;
            i := !i + 1
          done
        end else if sym = 17 then begin
          let repeat = read_bits r 3 + 3 in
          for _ = 1 to repeat do
            all_lengths.(!i) <- 0;
            i := !i + 1
          done
        end else begin
          let repeat = read_bits r 7 + 11 in
          for _ = 1 to repeat do
            all_lengths.(!i) <- 0;
            i := !i + 1
          done
        end
      done;
      let litlen_lengths = Array.sub all_lengths 0 hlit in
      let dist_lengths = Array.sub all_lengths hlit hdist in
      let litlen_table = build_huffman_table litlen_lengths hlit in
      let dist_table = build_huffman_table dist_lengths hdist in
      decode_lz litlen_table dist_table
    | _ -> failwith "TIFF: invalid deflate block type"
  done;
  Buffer.contents out_buf |> Bytes.of_string

let inflate data =
  let len = Bytes.length data in
  if len < 6 then failwith "TIFF: compressed data too short";
  let cmf = Bytes.get_uint8 data 0 in
  let flg = Bytes.get_uint8 data 1 in
  let cm = cmf land 0x0F in
  if cm <> 8 then failwith "TIFF: unsupported compression method";
  if (cmf * 256 + flg) mod 31 <> 0 then failwith "TIFF: invalid zlib header check";
  let has_dict = (flg land 0x20) <> 0 in
  if has_dict then failwith "TIFF: preset dictionary not supported";
  let raw_data = Bytes.sub data 2 (len - 6) in
  let result = inflate_raw raw_data in
  let expected = Bytes.get_int32_be data (len - 4) in
  let computed = adler32 result 0 (Bytes.length result) in
  if expected <> computed then
    failwith "TIFF: adler32 checksum mismatch";
  result

(* ---- Deflate (compression) ---- *)

type bit_writer = {
  mutable out : Buffer.t;
  mutable wbit_buf : int;
  mutable wbit_count : int;
}

let create_writer () = { out = Buffer.create 4096; wbit_buf = 0; wbit_count = 0 }

let write_bits w bits n =
  w.wbit_buf <- w.wbit_buf lor (bits lsl w.wbit_count);
  w.wbit_count <- w.wbit_count + n;
  while w.wbit_count >= 8 do
    Buffer.add_char w.out (Char.chr (w.wbit_buf land 0xFF));
    w.wbit_buf <- w.wbit_buf lsr 8;
    w.wbit_count <- w.wbit_count - 8
  done

let flush_bits w =
  if w.wbit_count > 0 then begin
    Buffer.add_char w.out (Char.chr (w.wbit_buf land 0xFF));
    w.wbit_buf <- 0;
    w.wbit_count <- 0
  end

let build_codes lengths n =
  let max_bits = 15 in
  let bl_count = Array.make (max_bits + 1) 0 in
  for i = 0 to n - 1 do
    if lengths.(i) > 0 then
      bl_count.(lengths.(i)) <- bl_count.(lengths.(i)) + 1
  done;
  let next_code = Array.make (max_bits + 1) 0 in
  let () =
    let rec loop bits code =
      if bits > max_bits then ()
      else begin
        let code = (code + bl_count.(bits - 1)) lsl 1 in
        next_code.(bits) <- code;
        loop (bits + 1) code
      end
    in
    loop 1 0
  in
  let codes = Array.make n 0 in
  for i = 0 to n - 1 do
    let len = lengths.(i) in
    if len > 0 then begin
      codes.(i) <- next_code.(len);
      next_code.(len) <- next_code.(len) + 1
    end
  done;
  codes

let build_codes_rev lengths n =
  let codes = build_codes lengths n in
  Array.init n (fun i ->
    if lengths.(i) > 0 then reverse_bits codes.(i) lengths.(i)
    else 0)

let deflate_litlen_lengths =
  let a = Array.make 288 0 in
  for i = 0 to 143 do a.(i) <- 8 done;
  for i = 144 to 255 do a.(i) <- 9 done;
  for i = 256 to 279 do a.(i) <- 7 done;
  for i = 280 to 287 do a.(i) <- 8 done;
  a

let deflate_litlen_codes = build_codes_rev deflate_litlen_lengths 288

let deflate_dist_lengths =
  let a = Array.make 30 0 in
  Array.fill a 0 30 5;
  a

let deflate_dist_codes = build_codes_rev deflate_dist_lengths 30

let find_length_code length =
  let rec search i =
    if i >= Array.length length_base - 1 then i
    else if length_base.(i + 1) > length then i
    else search (i + 1)
  in
  search 0

let find_dist_code dist =
  let rec search i =
    if i >= Array.length dist_base - 1 then i
    else if dist_base.(i + 1) > dist then i
    else search (i + 1)
  in
  search 0

let hash_size = 32768
let max_chain_length = 128

let hash3 buf pos len =
  if pos + 2 >= len then 0
  else
    let h = Bytes.get_uint8 buf pos in
    let h = h lxor (Bytes.get_uint8 buf (pos + 1) lsl 5) in
    let h = h lxor (Bytes.get_uint8 buf (pos + 2) lsl 10) in
    h land (hash_size - 1)

let find_match buf pos len head prev max_match_len =
  let max_dist = 32768 in
  let rec count_match mlen m max_l =
    if mlen < max_l &&
       Bytes.get_uint8 buf (pos + mlen) = Bytes.get_uint8 buf (m + mlen)
    then count_match (mlen + 1) m max_l
    else mlen
  in
  let h = hash3 buf pos len in
  let rec search m chain_len best_len best_dist =
    if m < 0 || chain_len >= max_chain_length || pos - m > max_dist then
      (best_len, best_dist)
    else
      let max_l = min max_match_len (min (len - pos) (len - m)) in
      let mlen = count_match 0 m max_l in
      if mlen > best_len then begin
        if mlen >= max_match_len then (mlen, pos - m)
        else search (prev.(m land (max_dist - 1))) (chain_len + 1) mlen (pos - m)
      end else
        search (prev.(m land (max_dist - 1))) (chain_len + 1) best_len best_dist
  in
  search head.(h) 0 2 0

let deflate_fixed data =
  let len = Bytes.length data in
  let w = create_writer () in
  let write_litlen sym =
    write_bits w deflate_litlen_codes.(sym) deflate_litlen_lengths.(sym)
  in
  let write_dist sym =
    write_bits w deflate_dist_codes.(sym) deflate_dist_lengths.(sym)
  in
  write_bits w 1 1;
  write_bits w 1 2;
  if len = 0 then begin
    write_litlen 256
  end else begin
    let head = Array.make hash_size (-1) in
    let prev = Array.make 32768 (-1) in
    let rec compress_loop pos =
      if pos >= len then ()
      else
        let max_match = min 258 (len - pos) in
        if max_match >= 3 then begin
          let mlen, mdist = find_match data pos len head prev max_match in
          if mlen >= 3 then begin
            let lcode = find_length_code mlen + 257 in
            write_litlen lcode;
            let lextra = length_extra.(lcode - 257) in
            if lextra > 0 then
              write_bits w (mlen - length_base.(lcode - 257)) lextra;
            let dcode = find_dist_code mdist in
            write_dist dcode;
            let dextra = dist_extra.(dcode) in
            if dextra > 0 then
              write_bits w (mdist - dist_base.(dcode)) dextra;
            for i = 0 to mlen - 1 do
              let p = pos + i in
              if p + 2 < len then begin
                let h = hash3 data p len in
                prev.(p land 32767) <- head.(h);
                head.(h) <- p
              end
            done;
            compress_loop (pos + mlen)
          end else begin
            let h = hash3 data pos len in
            prev.(pos land 32767) <- head.(h);
            head.(h) <- pos;
            write_litlen (Bytes.get_uint8 data pos);
            compress_loop (pos + 1)
          end
        end else begin
          write_litlen (Bytes.get_uint8 data pos);
          compress_loop (pos + 1)
        end
    in
    compress_loop 0;
    write_litlen 256
  end;
  flush_bits w;
  Buffer.contents w.out |> Bytes.of_string

let deflate ?(level = 6) data =
  let _ = level in
  let raw = deflate_fixed data in
  let result = Buffer.create (Bytes.length raw + 6) in
  let cmf = 0x78 in
  let flg_base = 0x01 in
  let check = (cmf * 256 + flg_base) mod 31 in
  let flg = if check = 0 then flg_base else flg_base + (31 - check) in
  Buffer.add_char result (Char.chr cmf);
  Buffer.add_char result (Char.chr flg);
  Buffer.add_bytes result raw;
  let checksum = adler32 data 0 (Bytes.length data) in
  Buffer.add_char result (Char.chr (Int32.to_int (Int32.shift_right_logical checksum 24) land 0xFF));
  Buffer.add_char result (Char.chr (Int32.to_int (Int32.shift_right_logical checksum 16) land 0xFF));
  Buffer.add_char result (Char.chr (Int32.to_int (Int32.shift_right_logical checksum 8) land 0xFF));
  Buffer.add_char result (Char.chr (Int32.to_int checksum land 0xFF));
  Buffer.contents result |> Bytes.of_string
