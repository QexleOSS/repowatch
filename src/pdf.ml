(* PDF text and topic extraction, offline. Streams compressed with FlateDecode
   go through Inflate; other filters fall back to the raw bytes. *)

let find_sub s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go from

let printable s =
  let ok = ref true in
  String.iter (fun c ->
    let v = Char.code c in
    if v < 32 && c <> '\n' && c <> '\t' then ok := false) s;
  !ok

let extract_strings data out =
  let n = String.length data in
  let rec go i =
    if i >= n then ()
    else if data.[i] = '(' then begin
      let j = ref (i + 1) and esc = ref false in
      let buf = Buffer.create 64 in
      while !j < n && (!esc || data.[!j] <> ')') do
        let c = data.[!j] in
        if !esc then begin Buffer.add_char buf c; esc := false end
        else if c = '\\' then esc := true
        else Buffer.add_char buf c;
        incr j
      done;
      let s = String.trim (Buffer.contents buf) in
      if String.length s > 1 && printable s then begin
        Buffer.add_string out s;
        Buffer.add_char out ' '
      end;
      go (!j + 1)
    end else go (i + 1)
  in
  go 0

let count_pages content =
  let n = String.length content in
  let rec go i acc =
    if i + 11 > n then acc
    else if String.sub content i 11 = "/Type /Page" then
      let is_pages = i + 11 < n && content.[i + 11] = 's' in
      go (i + 11) (if is_pages then acc else acc + 1)
    else go (i + 1) acc
  in
  max 1 (go 0 0)

let ascii85 s =
  let out = Buffer.create (String.length s * 4 / 5) in
  let tuple = Array.make 5 0 and count = ref 0 in
  let emit n =
    let v = ref 0 in
    for i = 0 to 4 do v := !v * 85 + tuple.(i) done;
    let bytes = [| (!v lsr 24) land 255; (!v lsr 16) land 255;
                  (!v lsr 8) land 255; !v land 255 |] in
    for i = 0 to n - 1 do Buffer.add_char out (Char.chr bytes.(i)) done
  in
  String.iter (fun c ->
    if c = 'z' && !count = 0 then Buffer.add_string out "\000\000\000\000"
    else if c >= '!' && c <= 'u' then begin
      tuple.(!count) <- Char.code c - 33;
      incr count;
      if !count = 5 then (emit 4; count := 0)
    end) s;
  if !count > 0 then begin
    for i = !count to 4 do tuple.(i) <- 84 done;
    if !count > 1 then emit (!count - 1)
  end;
  Buffer.contents out

let text content =
  let out = Buffer.create 4096 in
  let n = String.length content in
  let rec scan i =
    if i + 6 > n then ()
    else if String.sub content i 6 = "stream" then begin
      let start =
        if i + 7 < n && content.[i + 6] = '\r' && content.[i + 7] = '\n' then i + 8
        else if i + 6 < n && (content.[i + 6] = '\n' || content.[i + 6] = '\r') then i + 7
        else i + 6
      in
      match find_sub content "endstream" start with
      | None -> ()
      | Some stop ->
        let raw = String.sub content start (stop - start) in
        let head = String.sub content (max 0 (i - 256)) (min 256 i) in
        let flate = find_sub head "/FlateDecode" 0 <> None in
        let data =
          let d = if find_sub head "/ASCII85Decode" 0 <> None
            then (try ascii85 raw with _ -> raw) else raw in
          if flate then (try Inflate.inflate d with _ -> d) else d
        in
        extract_strings data out;
        scan (stop + 9)
    end else scan (i + 1)
  in
  scan 0;
  Buffer.contents out

let stopwords =
  [ "the"; "and"; "for"; "with"; "this"; "that"; "from"; "have"; "will";
    "your"; "their"; "there"; "which"; "about"; "into"; "when"; "where";
    "what"; "when"; "been"; "were"; "they"; "than"; "then"; "some"; "such" ]

let topics content limit =
  let counts = Hashtbl.create 64 in
  let word = Buffer.create 32 in
  let flush () =
    let w = String.lowercase_ascii (Buffer.contents word) in
    Buffer.clear word;
    if String.length w >= 5 && not (List.mem w stopwords) then
      Hashtbl.replace counts w
        (1 + Option.value ~default:0 (Hashtbl.find_opt counts w))
  in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then Buffer.add_char word c
    else flush ()) content;
  flush ();
  Hashtbl.fold (fun w c acc -> (w, c) :: acc) counts []
  |> List.sort (fun (a, c) (b, d) -> compare d c |> fun x -> if x <> 0 then x else compare a b)
  |> List.filteri (fun i _ -> i < limit)

let extract content =
  let body = text content in
  (body, count_pages content, topics body 8)
