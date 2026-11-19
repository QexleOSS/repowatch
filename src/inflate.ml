(* Minimal DEFLATE decoder, used to read compressed PDF streams.
   No zlib, no C: about 120 lines of canonical Huffman. *)

exception Bad

type bits = { s : string; mutable pos : int; mutable bit : int }

let get_bit b =
  if b.pos >= String.length b.s then raise Bad;
  let v = (Char.code b.s.[b.pos] lsr b.bit) land 1 in
  b.bit <- b.bit + 1;
  if b.bit = 8 then begin b.bit <- 0; b.pos <- b.pos + 1 end;
  v

let get_bits b n =
  let v = ref 0 in
  for i = 0 to n - 1 do v := !v lor (get_bit b lsl i) done;
  !v

type huff = { count : int array; symbols : int array }

let build lengths =
  let maxbits = 15 in
  let count = Array.make (maxbits + 1) 0 in
  Array.iter (fun l -> if l > 0 && l <= maxbits then count.(l) <- count.(l) + 1) lengths;
  let offs = Array.make (maxbits + 1) 0 in
  for l = 1 to maxbits - 1 do offs.(l + 1) <- offs.(l) + count.(l) done;
  let symbols = Array.make (Array.length lengths) 0 in
  Array.iteri (fun i l ->
    if l > 0 then begin symbols.(offs.(l)) <- i; offs.(l) <- offs.(l) + 1 end) lengths;
  { count; symbols }

let decode h b =
  let rec go len code first index =
    if len > 15 then raise Bad;
    let code = code lor get_bit b in
    let count = h.count.(len) in
    if code - first < count then h.symbols.(index + code - first)
    else go (len + 1) (code lsl 1) ((first + count) lsl 1) (index + count)
  in
  go 1 0 0 0

let fixed_lit () =
  let l = Array.make 288 0 in
  for i = 0 to 143 do l.(i) <- 8 done;
  for i = 144 to 255 do l.(i) <- 9 done;
  for i = 256 to 279 do l.(i) <- 7 done;
  for i = 280 to 287 do l.(i) <- 8 done;
  build l

let len_base = [|3;4;5;6;7;8;9;10;11;13;15;17;19;23;27;31;35;43;51;59;67;83;99;115;131;163;195;227;258|]
let len_extra = [|0;0;0;0;0;0;0;0;1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4;5;5;5;5;0|]
let dist_base = [|1;2;3;4;5;7;9;13;17;25;33;49;65;97;129;193;257;385;513;769;1025;1537;2049;3073;4097;6145;8193;12289;16385;24577|]
let dist_extra = [|0;0;0;0;1;1;2;2;3;3;4;4;5;5;6;6;7;7;8;8;9;9;10;10;11;11;12;12;13;13|]

let inflate s =
  let start = if String.length s > 2 && (Char.code s.[0] land 0x0f) = 8 then 2 else 0 in
  let b = { s; pos = start; bit = 0 } in
  let out = ref (Bytes.create (max 64 (String.length s * 3))) in
  let len = ref 0 in
  let ensure n =
    if !len + n > Bytes.length !out then begin
      let cap = ref (Bytes.length !out) in
      while !len + n > !cap do cap := !cap * 2 done;
      let nb = Bytes.create !cap in
      Bytes.blit !out 0 nb 0 !len;
      out := nb
    end
  in
  let put c = ensure 1; Bytes.set !out !len (Char.chr (c land 255)); incr len in
  let copy d l =
    if d <= 0 || d > !len then raise Bad;
    for _ = 1 to l do put (Char.code (Bytes.get !out (!len - d))) done
  in
  let dynamic () =
    let order = [|16;17;18;0;8;7;9;6;10;5;11;4;12;3;13;2;14;1;15|] in
    let hclen = get_bits b 4 + 4 in
    let clen = Array.make 19 0 in
    for i = 0 to hclen - 1 do clen.(order.(i)) <- get_bits b 3 done;
    let clh = build clen in
    let hlit = get_bits b 5 + 257 and hdist = get_bits b 5 + 1 in
    let total = hlit + hdist in
    let lens = Array.make total 0 in
    let i = ref 0 in
    while !i < total do
      match decode clh b with
      | 16 ->
        if !i = 0 then raise Bad;
        let prev = lens.(!i - 1) and n = get_bits b 2 + 3 in
        for _ = 1 to n do if !i < total then begin lens.(!i) <- prev; incr i end done
      | 17 ->
        let n = get_bits b 3 + 3 in
        for _ = 1 to n do if !i < total then incr i done
      | 18 ->
        let n = get_bits b 7 + 11 in
        for _ = 1 to n do if !i < total then incr i done
      | c -> lens.(!i) <- c; incr i
    done;
    let lit = build (Array.sub lens 0 hlit) in
    let dist = build (Array.sub lens hlit hdist) in
    (lit, dist)
  in
  let codes lit dist =
    let rec loop () =
      let sym = decode lit b in
      if sym < 256 then begin put sym; loop () end
      else if sym = 256 then ()
      else begin
        let idx = sym - 257 in
        if idx < 0 || idx >= 29 then raise Bad;
        let l = len_base.(idx) + get_bits b len_extra.(idx) in
        let dsym = decode dist b in
        if dsym >= 30 then raise Bad;
        let d = dist_base.(dsym) + get_bits b dist_extra.(dsym) in
        copy d l;
        loop ()
      end
    in
    loop ()
  in
  let rec blocks () =
    let final = get_bit b in
    (match get_bits b 2 with
    | 0 ->
      if b.bit <> 0 then begin b.bit <- 0; b.pos <- b.pos + 1 end;
      if b.pos + 4 > String.length b.s then raise Bad;
      let n = Char.code b.s.[b.pos] lor (Char.code b.s.[b.pos + 1] lsl 8) in
      b.pos <- b.pos + 4;
      if b.pos + n > String.length b.s then raise Bad;
      ensure n;
      for k = 0 to n - 1 do put (Char.code b.s.[b.pos + k]) done;
      b.pos <- b.pos + n
    | 1 ->
      let lit = fixed_lit () and dist = build (Array.make 32 5) in
      codes lit dist
    | 2 ->
      let lit, dist = dynamic () in
      codes lit dist
    | _ -> raise Bad);
    if final = 0 then blocks ()
  in
  blocks ();
  Bytes.sub_string !out 0 !len
