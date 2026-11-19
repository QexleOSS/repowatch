(* repowatch: a ranked structural index of a repository.
   Commands: map, stats, refs, why, set, check, snapshot, drift. *)

let version = "3.0.0"

let root = ref (Unix.getcwd ())
let budget = ref 4000
let json = ref false

let usage = {|
repowatch: the shape of a repository

  repowatch map [--root DIR] [--budget N]   ranked tree of every file
  repowatch stats                           index totals and language tiers
  repowatch refs NAME                       who imports or calls a symbol
  repowatch why PATH                        why a file sits where it does
  repowatch set TASK [--budget N]           files you must read, with a receipt
  repowatch check                           cycles and size limits, exits non-zero
  repowatch snapshot NAME                   record the current shape
  repowatch drift [NAME]                    what changed since a snapshot
  repowatch tags [--out PATH]                tags file for your editor
  repowatch graph [--out PATH]               self-contained HTML graph
  repowatch wiki [--out DIR]                 markdown pages for Obsidian
  repowatch watch [--interval S]             rebuild when files change
  repowatch lsp                             language server on stdio
  repowatch version
  add --json to any command for machine-readable output

No network. No account. The index lives in .repowatch/index.
|}

let thousands n =
  let s = string_of_int n in
  let len = String.length s in
  let b = Buffer.create len in
  String.iteri (fun i c ->
    if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ',';
    Buffer.add_char b c
  ) s;
  Buffer.contents b

let count_lines content =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) content;
  !n

(* --- json --------------------------------------------------------------- *)

let esc s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\t' -> Buffer.add_string b "\\t"
    | c when Char.code c < 32 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let q s = "\"" ^ esc s ^ "\""
let jarr items = "[" ^ String.concat "," items ^ "]"
let jobj fields =
  "{" ^ String.concat "," (List.map (fun (k, v) -> q k ^ ":" ^ v) fields) ^ "}"
let jint n = string_of_int n
let emit o = print_endline o

(* --- build ------------------------------------------------------------- *)

let build root =
  let old = Index.load root in
  let cache = Hashtbl.create 256 in
  List.iter (fun f -> Hashtbl.replace cache f.Index.path f) old;
  let files =
    Walk.walk root
    |> List.filter_map (fun (rel, st) ->
      match Lang.of_path rel with
      | None -> None
      | Some lang ->
        match Hashtbl.find_opt cache rel with
        | Some f when f.Index.size = st.Unix.st_size
                   && abs_float (f.Index.mtime -. st.Unix.st_mtime) < 1.0 -> Some f
        | _ ->
          let content =
            In_channel.with_open_bin (Filename.concat root rel) In_channel.input_all
          in
          let lang =
            if lang = "objc" && not (Lex.contains content "#import"
                                      || Lex.contains content "@interface"
                                      || Lex.contains content "@implementation"
                                      || Lex.contains content "@end")
            then "matlab"
            else if lang = "c" && (Lex.contains content "namespace "
                                   || Lex.contains content "template<"
                                   || Lex.contains content "std::")
            then "cpp"
            else lang
          in
          let r =
            match Lang.tier lang with
            | Lang.Files -> { Lex.imports = []; calls = []; symbols = [] }
            | _ -> Lex.extract lang content
          in
          Some {
            Index.path = rel; lang; size = st.Unix.st_size; mtime = st.Unix.st_mtime;
            lines = count_lines content;
            hash = Digest.to_hex (Digest.string content);
            imports = r.Lex.imports; calls = r.Lex.calls; symbols = r.Lex.symbols;
          })
  in
  Index.save root files;
  files

(* --- resolution and graph ---------------------------------------------- *)

let strip_ext p =
  match Filename.chop_extension p with p -> p | exception Invalid_argument _ -> p

let stem path =
  String.lowercase_ascii (Filename.remove_extension (Filename.basename path))

let last_segment lang spec =
  let spec = String.trim spec in
  let cut chars =
    let idx = ref (-1) in
    String.iteri (fun i c -> if List.mem c chars then idx := i) spec;
    if !idx >= 0 then String.sub spec (!idx + 1) (String.length spec - !idx - 1) else spec
  in
  let s =
    match lang with
    | "typescript" | "tsx" | "javascript" -> cut [ '/'; '\\' ]
    | "python" -> cut [ '.'; '/' ]
    | "rust" -> cut [ ':'; '/' ]
    | "perl" -> cut [ ':'; '/' ]
    | "go" -> cut [ '/' ]
    | "java" | "c_sharp" | "ada" | "kotlin" | "scala" | "julia" | "elixir"
    | "fsharp" | "groovy" | "d" | "vb" | "razor" | "xaml" | "elm" | "vala" | "apex" -> cut [ '.'; '\\'; '/' ]
    | "ocaml" -> cut [ '.' ]
    | "c" | "cpp" | "markdown" | "sql" | "protobuf" | "objc" | "dart" | "swift"
    | "erlang" | "r" | "solidity" | "vue" | "svelte"
    | "nix" | "graphql" | "nim" | "crystal" | "bash" | "pascal" | "fortran"
    | "verilog" | "vhdl" | "starlark" | "cmake" | "asm" | "prolog" | "lisp"
    | "msbuild" | "sln" | "cobol" | "abap" | "matlab" | "glsl" | "hlsl" | "opencl" -> Filename.basename spec
    | _ -> cut [ '/'; '.' ]
  in
  String.lowercase_ascii (strip_ext (Filename.basename s))

let norm_path p =
  let parts = String.split_on_char '/' p in
  let out = List.fold_left (fun acc part ->
    match part with
    | "" | "." -> acc
    | ".." -> (match acc with _ :: rest -> rest | [] -> [])
    | x -> x :: acc) [] parts
  in
  String.concat "/" (List.rev out)

let ts_aliases () =
  let read name =
    let p = Filename.concat !root name in
    match In_channel.with_open_bin p In_channel.input_all with
    | exception Sys_error _ -> None
    | s -> (try Some (Json.parse s) with _ -> None)
  in
  let conf = match read "tsconfig.json" with Some c -> Some c | None -> read "jsconfig.json" in
  match conf with
  | None -> ([], ".")
  | Some c ->
    let opts = match Json.member "compilerOptions" c with Some o -> o | None -> Json.Null in
    let base = match Json.member "baseUrl" opts with Some (Json.Str s) -> s | _ -> "." in
    let paths = match Json.member "paths" opts with Some (Json.Obj xs) -> xs | _ -> [] in
    let pairs = List.filter_map (fun (k, v) ->
      match v with Json.Arr (Json.Str t :: _) -> Some (k, t) | _ -> None) paths in
    (pairs, base)

let build_graph files =
  let arr = Array.of_list files in
  let n = Array.length arr in
  let by_path = Hashtbl.create n and by_stem = Hashtbl.create n
  and by_symbol = Hashtbl.create n and by_noext = Hashtbl.create n in
  Array.iteri (fun i f ->
    Hashtbl.replace by_path f.Index.path i;
    Hashtbl.replace by_noext (String.lowercase_ascii (strip_ext f.Index.path))
      (i :: Option.value ~default:[] (Hashtbl.find_opt by_noext (String.lowercase_ascii (strip_ext f.Index.path))));
    let key = stem f.Index.path in
    Hashtbl.replace by_stem key (i :: Option.value ~default:[] (Hashtbl.find_opt by_stem key));
    List.iter (fun s ->
      let k = String.lowercase_ascii s.Lex.name in
      Hashtbl.replace by_symbol k (i :: Option.value ~default:[] (Hashtbl.find_opt by_symbol k))
    ) f.Index.symbols
  ) arr;
  let aliases, base_url = ts_aliases () in
  let by_noext_lookup path =
    match Hashtbl.find_opt by_noext (String.lowercase_ascii (strip_ext path)) with
    | Some (j :: _) -> Some j
    | _ -> None
  in
  let expand pat target spec =
    match String.index_opt pat '*' with
    | None -> if spec = pat then Some target else None
    | Some star ->
      let pre = String.sub pat 0 star in
      let post = String.sub pat (star + 1) (String.length pat - star - 1) in
      if Lex.starts spec pre
         && String.length spec >= String.length pre + String.length post
         && String.sub spec (String.length spec - String.length pre - String.length post)
              (String.length post) = post then
        let mid = String.sub spec (String.length pre)
            (String.length spec - String.length pre - String.length post) in
        (match String.index_opt target '*' with
         | Some t ->
           Some (String.sub target 0 t ^ mid
                 ^ String.sub target (t + 1) (String.length target - t - 1))
         | None -> Some target)
      else None
  in
  let alias_lookup spec =
    let rec go = function
      | [] -> None
      | (pat, target) :: rest ->
        (match expand pat target spec with
         | Some path -> by_noext_lookup (norm_path (Filename.concat base_url path))
         | None -> go rest)
    in
    go aliases
  in
  let relative_lookup f spec =
    if spec <> "" && spec.[0] = '.' then
      by_noext_lookup (norm_path (Filename.concat (Filename.dirname f.Index.path) spec))
    else None
  in
  let resolve src f spec =
    let direct =
      match alias_lookup spec with
      | Some j -> Some j
      | None -> relative_lookup f spec
    in
    match direct with
    | Some j -> Some j
    | None ->
    let needle = last_segment f.Index.lang spec in
    match Hashtbl.find_opt by_stem needle with
    | None -> None
    | Some [ j ] -> Some j
    | Some candidates ->
      let dir = Filename.dirname f.Index.path in
      let wanted = String.lowercase_ascii (Filename.concat dir (needle)) in
      (match List.find_opt (fun j ->
         String.lowercase_ascii (strip_ext arr.(j).Index.path) = wanted
       ) candidates with
       | Some j -> Some j
       | None -> Some (List.hd candidates))
  in
  let edges = ref [] and seen = Hashtbl.create 1024 in
  let add src dst kind =
    if src <> dst then begin
      let key = (src, dst, kind) in
      if not (Hashtbl.mem seen key) then begin
        Hashtbl.replace seen key ();
        edges := { Graph.src; dst; kind } :: !edges
      end
    end
  in
  Array.iteri (fun i f ->
    List.iter (fun spec ->
      match resolve i f spec with Some j -> add i j 0 | None -> ()
    ) f.Index.imports;
    List.iter (fun call ->
      let k = String.lowercase_ascii call in
      match Hashtbl.find_opt by_symbol k with
      | Some [ j ] -> add i j 1
      | Some js ->
        let dir = Filename.dirname f.Index.path in
        let pick =
          match List.find_opt (fun j -> Filename.dirname arr.(j).Index.path = dir) js with
          | Some j -> j
          | None -> List.hd js
        in
        add i pick 1
      | None -> ()
    ) f.Index.calls
  ) arr;
  let edges = Array.of_list !edges in
  (arr, by_path, by_symbol, edges, Graph.pagerank n edges, Graph.in_degree n edges)

(* --- lsp ---------------------------------------------------------------- *)

let lsp_uri path =
  "file://" ^ (if Filename.is_relative path then Filename.concat !root path else path)

let path_of_uri uri =
  if Lex.starts uri "file://" then String.sub uri 7 (String.length uri - 7) else uri

let lsp_kind = function
  | "class" -> 5 | "interface" -> 11 | "struct" -> 23 | "enum" -> 10
  | "method" -> 6 | "function" -> 12 | "module" -> 2 | _ -> 13

let location path line =
  let l = max 0 (line - 1) in
  jobj [ "uri", q (lsp_uri path);
         "range", jobj [ "start", jobj [ "line", jint l; "character", "0" ];
                         "end", jobj [ "line", jint l; "character", "1" ] ] ]

let word_at path line character =
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error _ -> ""
  | content ->
    (match List.nth_opt (String.split_on_char '\n' content) line with
     | None -> ""
     | Some text ->
       let n = String.length text in
       if n = 0 then ""
       else begin
         let is_id c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                       || (c >= '0' && c <= '9') || c = '_' || c = '\'' in
         let pos = min (max 0 character) (n - 1) in
         if not (is_id text.[pos]) then ""
         else begin
           let a = ref pos and b = ref pos in
           while !a > 0 && is_id text.[!a - 1] do decr a done;
           while !b + 1 < n && is_id text.[!b + 1] do incr b done;
           String.sub text !a (!b - !a + 1)
         end
       end)

let serve_lsp () =
  set_binary_mode_in stdin true;
  set_binary_mode_out stdout true;
  let arr = ref [||] and edges = ref [||] in
  let ensure () =
    if Array.length !arr = 0 then begin
      let files = build !root in
      let a, _, _, e, _, _ = build_graph files in
      arr := a;
      edges := e
    end
  in
  let send s = Printf.printf "Content-Length: %d\r\n\r\n%s%!" (String.length s) s in
  let id_of = function
    | Json.Num n -> Printf.sprintf "%.0f" n
    | Json.Str s -> q s
    | _ -> "null"
  in
  let defs_of name =
    let target = String.lowercase_ascii name in
    let out = ref [] in
    Array.iteri (fun i f ->
      List.iter (fun s ->
        if String.lowercase_ascii s.Lex.name = target then
          out := (i, f.Index.path, s.Lex.line) :: !out) f.Index.symbols) !arr;
    !out
  in
  let read () =
    let len = ref 0 in
    let rec headers () =
      match input_line stdin with
      | exception End_of_file -> raise Exit
      | line ->
        let line = String.trim line in
        if line = "" then ()
        else begin
          if Lex.starts (String.lowercase_ascii line) "content-length:" then
            len := int_of_string (String.trim (String.sub line 15 (String.length line - 15)));
          headers ()
        end
    in
    headers ();
    really_input_string stdin !len
  in
  let respond id result =
    match id with
    | None -> ()
    | Some id ->
      send (jobj [ "jsonrpc", q "2.0"; "id", id_of id; "result", result ])
  in
  let handle msg =
    let id = Json.member "id" msg in
    let method_ = match Json.member "method" msg with Some (Json.Str m) -> m | _ -> "" in
    let params = Json.member "params" msg in
    let get k = match params with Some p -> Json.member k p | None -> None in
    let at_position () =
      let path = match get "textDocument" with
        | Some td -> (match Json.member "uri" td with Some (Json.Str u) -> path_of_uri u | _ -> "")
        | None -> ""
      in
      let line = match get "position" with
        | Some p -> (match Json.member "line" p with Some (Json.Num n) -> int_of_float n | _ -> -1)
        | None -> -1
      in
      let ch = match get "position" with
        | Some p -> (match Json.member "character" p with Some (Json.Num n) -> int_of_float n | _ -> 0)
        | None -> 0
      in
      (path, line, ch)
    in
    match method_ with
    | "initialize" ->
      (match get "rootUri" with
       | Some (Json.Str uri) when uri <> "" -> root := path_of_uri uri
       | _ -> ());
      respond id (jobj [ "capabilities", jobj [
        "textDocumentSync", "1";
        "definitionProvider", "true";
        "referencesProvider", "true";
        "workspaceSymbolProvider", "true" ] ])
    | "shutdown" -> respond id "null"
    | "exit" -> raise Exit
    | "textDocument/definition" ->
      ensure ();
      let path, line, ch = at_position () in
      let defs = defs_of (if path = "" then "" else word_at path line ch) in
      respond id (if defs = [] then "null"
                  else jarr (List.map (fun (_, p, l) -> location p l) defs))
    | "textDocument/references" ->
      ensure ();
      let path, line, ch = at_position () in
      let defs = defs_of (if path = "" then "" else word_at path line ch) in
      let indices = List.map (fun (i, _, _) -> i) defs in
      let rows = ref (List.map (fun (_, p, l) -> location p l) defs) in
      let seen = Hashtbl.create 16 in
      Array.iter (fun e ->
        if List.mem e.Graph.dst indices then begin
          let path = !arr.(e.Graph.src).Index.path in
          if not (Hashtbl.mem seen path) then begin
            Hashtbl.replace seen path ();
            rows := location path 1 :: !rows
          end
        end) !edges;
      respond id (jarr !rows)
    | "workspace/symbol" ->
      ensure ();
      let query = match get "query" with
        | Some (Json.Str s) -> String.lowercase_ascii s
        | _ -> ""
      in
      let out = ref [] and count = ref 0 in
      Array.iter (fun f ->
        List.iter (fun s ->
          if !count < 200
             && (query = "" || Lex.contains (String.lowercase_ascii s.Lex.name) query) then begin
            incr count;
            out := jobj [ "name", q s.Lex.name; "kind", jint (lsp_kind s.Lex.kind);
                          "location", location f.Index.path s.Lex.line ] :: !out
          end) f.Index.symbols) !arr;
      respond id (jarr !out)
    | _ ->
      (match id with Some _ -> respond id "null" | None -> ())
  in
  (try
     while true do
       let body = read () in
       (try handle (Json.parse body) with
        | Exit -> raise Exit
        | _ -> ())
     done
   with Exit -> ());
  0

(* --- tree rendering ----------------------------------------------------- *)

type node = {
  name : string;
  mutable kids : node list;
  mutable leaf : (float * int * int) option; (* score, symbols, lines *)
  mutable best : float;
}

let empty_node name = { name; kids = []; leaf = None; best = 0.0 }

let insert root path score syms lines =
  let parts = String.split_on_char '/' path in
  let rec go node = function
    | [] -> ()
    | [ last ] ->
      node.kids <- node.kids @ [ { name = last; kids = []; leaf = Some (score, syms, lines); best = score } ]
    | part :: rest ->
      node.best <- max node.best score;
      match List.find_opt (fun k -> k.name = part && k.leaf = None) node.kids with
      | Some kid -> go kid rest
      | None ->
        let kid = { name = part; kids = []; leaf = None; best = score } in
        node.kids <- node.kids @ [ kid ];
        go kid rest
  in
  go root parts

let rank_marks score best =
  if best <= 0.0 then ""
  else
    let relative = score /. best in
    let bars = int_of_float (ceil (relative *. 5.0)) in
    String.make (max 1 (min 5 bars)) '#'

let render_tree root =
  let best = root.best in
  let b = Buffer.create 4096 in
  let rec go node prefix =
    let kids =
      List.sort (fun a c -> compare c.best a.best) node.kids
    in
    List.iteri (fun i kid ->
      let last = i = List.length kids - 1 in
      let branch = if last then "└─ " else "├─ " in
      let child_prefix = prefix ^ (if last then "   " else "│  ") in
      Buffer.add_string b (prefix ^ branch ^ kid.name);
      (match kid.leaf with
       | Some (score, syms, lines) ->
         Buffer.add_string b
           (Printf.sprintf "  %-5s %.3f  %3d sym  %5s ln"
              (rank_marks score best) score syms (thousands lines))
       | None -> ());
      Buffer.add_char b '\n';
      go kid child_prefix
    ) kids
  in
  go root "";
  Buffer.contents b

(* --- commands ----------------------------------------------------------- *)

let arg_value name default =
  let rec go = function
    | [] -> default
    | a :: b :: _ when a = name -> b
    | _ :: rest -> go rest
  in
  go (List.tl (Array.to_list Sys.argv))

let command_map files =
  let arr, _, _, edges, scores, _ = build_graph files in
  let order = List.init (Array.length arr) Fun.id
    |> List.sort (fun a b ->
      let c = compare scores.(b) scores.(a) in
      if c <> 0 then c else compare arr.(a).Index.path arr.(b).Index.path)
  in
  let total_lines = Array.fold_left (fun s f -> s + f.Index.lines) 0 arr in
  let total_syms = Array.fold_left (fun s f -> s + List.length f.Index.symbols) 0 arr in
  if !json then begin
    let rows = List.mapi (fun r i ->
      let f = arr.(i) in
      jobj [ "rank", jint (r + 1); "path", q f.Index.path;
             "score", Printf.sprintf "%.4f" scores.(i);
             "symbols", jint (List.length f.Index.symbols);
             "lines", jint f.Index.lines; "lang", q f.Index.lang ]) order
    in
    emit (jobj [ "root", q !root;
                 "totals", jobj [ "files", jint (Array.length arr);
                                  "symbols", jint total_syms; "lines", jint total_lines;
                                  "edges", jint (Array.length edges) ];
                 "files", jarr rows ])
  end else begin
    let tree = empty_node "" in
    let shown = ref 0 and used = ref 0 in
    List.iter (fun i ->
      let f = arr.(i) in
      if !used + f.Index.lines <= !budget then begin
        used := !used + f.Index.lines;
        incr shown;
        insert tree f.Index.path scores.(i) (List.length f.Index.symbols) f.Index.lines
      end
    ) order;
    Printf.printf "repowatch  %s\n" !root;
    Printf.printf "%s files · %s symbols · %s lines · %s edges\n\n"
      (thousands (Array.length arr)) (thousands total_syms)
      (thousands total_lines) (thousands (Array.length edges));
    Printf.printf "%s\n" (render_tree tree);
    if !shown < Array.length arr then
      Printf.printf "%d files omitted to stay inside %s lines\n"
        (Array.length arr - !shown) (thousands !budget)
  end

let command_stats files =
  let arr = Array.of_list files in
  let total_lines = Array.fold_left (fun s f -> s + f.Index.lines) 0 arr in
  let total_syms = Array.fold_left (fun s f -> s + List.length f.Index.symbols) 0 arr in
  let langs = Hashtbl.create 16 in
  Array.iter (fun f ->
    let n = Option.value ~default:0 (Hashtbl.find_opt langs f.Index.lang) in
    Hashtbl.replace langs f.Index.lang (n + 1)
  ) arr;
  let rows = Hashtbl.fold (fun lang n acc -> (lang, n) :: acc) langs [] in
  let rows = List.sort (fun (_, a) (_, b) -> compare b a) rows in
  if !json then
    emit (jobj [ "files", jint (Array.length arr); "symbols", jint total_syms;
                 "lines", jint total_lines;
                 "languages", jarr (List.map (fun (lang, n) ->
                   jobj [ "language", q lang; "files", jint n;
                          "tier", q (Lang.label (Lang.tier lang)) ]) rows) ])
  else begin
    Printf.printf "%s files  %s symbols  %s lines  index\n"
      (thousands (Array.length arr)) (thousands total_syms) (thousands total_lines);
    List.iter (fun (lang, n) ->
      Printf.printf "  %-12s %4d files  %s\n" lang n (Lang.label (Lang.tier lang))) rows
  end

let command_refs files name =
  let arr, by_path, by_symbol, edges, _, _ = build_graph files in
  let target = String.lowercase_ascii name in
  let defs = Option.value ~default:[] (Hashtbl.find_opt by_symbol target) in
  if defs = [] then begin
    if !json then emit (jobj [ "name", q name; "definitions", "[]"; "callers", "[]" ])
    else Printf.eprintf "nothing defines %s\n" name;
    1
  end else begin
    let found =
      List.concat_map (fun i ->
        let f = arr.(i) in
        List.filter_map (fun s ->
          if String.lowercase_ascii s.Lex.name = target then Some (f.Index.path, s)
          else None) f.Index.symbols) defs
    in
    let callers = ref [] in
    Array.iter (fun e ->
      if List.mem e.Graph.dst defs && e.Graph.kind = 1 then
        callers := arr.(e.Graph.src).Index.path :: !callers
    ) edges;
    let callers = List.sort_uniq compare !callers in
    if !json then
      emit (jobj [ "name", q name;
                   "definitions", jarr (List.map (fun (path, s) ->
                     jobj [ "path", q path; "line", jint s.Lex.line;
                            "kind", q s.Lex.kind; "exported", if s.Lex.exported then "true" else "false" ]) found);
                   "callers", jarr (List.map q callers) ])
    else begin
      List.iter (fun (path, s) ->
        Printf.printf "define  %-10s %s:%d\n" s.Lex.kind path s.Lex.line) found;
      List.iter (fun p -> Printf.printf "call    %s\n" p) callers
    end;
    ignore by_path;
    0
  end

let command_why files path =
  let arr, _, _, edges, scores, indeg = build_graph files in
  match Array.to_list arr |> List.mapi (fun i f -> (i, f)) |> List.find_opt (fun (_, f) -> f.Index.path = path) with
  | None ->
    if !json then emit (jobj [ "path", q path; "error", q "not indexed" ])
    else Printf.eprintf "not indexed: %s\n" path;
    1
  | Some (i, f) ->
    let out = Array.to_list edges |> List.filter (fun e -> e.Graph.src = i) |> List.length in
    let role =
      if indeg.(i) = 0 && out = 0 then "unused"
      else if indeg.(i) = 0 then "entry point"
      else if out = 0 then "leaf"
      else "internal"
    in
    if !json then
      emit (jobj [ "path", q f.Index.path; "role", q role;
                   "score", Printf.sprintf "%.4f" scores.(i);
                   "imported_by", jint indeg.(i); "depends_on", jint out;
                   "language", q f.Index.lang;
                   "tier", q (Lang.label (Lang.tier f.Index.lang));
                   "lines", jint f.Index.lines;
                   "symbols", jarr (List.map (fun s ->
                     jobj [ "name", q s.Lex.name; "kind", q s.Lex.kind;
                            "line", jint s.Lex.line;
                            "exported", if s.Lex.exported then "true" else "false" ])
                     f.Index.symbols) ])
    else begin
      Printf.printf "%s\n" f.Index.path;
      Printf.printf "  role         %s\n" role;
      Printf.printf "  score        %.4f\n" scores.(i);
      Printf.printf "  imported by  %d\n" indeg.(i);
      Printf.printf "  depends on   %d\n" out;
      Printf.printf "  language     %s (%s)\n" f.Index.lang (Lang.label (Lang.tier f.Index.lang));
      Printf.printf "  lines       %s\n" (thousands f.Index.lines);
      Printf.printf "  symbols      %d\n" (List.length f.Index.symbols);
      List.iter (fun s ->
        Printf.printf "    %-9s %s%s (line %d)\n" s.Lex.kind
          (if s.Lex.exported then "" else "~") s.Lex.name s.Lex.line
      ) (List.filteri (fun k _ -> k < 15) f.Index.symbols)
    end;
    0

(* --- read set and checks ------------------------------------------------ *)

let stopwords =
  [ "the"; "a"; "an"; "to"; "of"; "in"; "on"; "for"; "and"; "or"; "with";
    "be"; "is"; "are"; "was"; "were"; "that"; "this"; "these"; "those";
    "it"; "its"; "when"; "into"; "from"; "how"; "why"; "what"; "should" ]

let is_alnum c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let terms_of task =
  let b = Buffer.create 32 in
  String.iter (fun c ->
    Buffer.add_char b (if is_alnum c then Char.lowercase_ascii c else ' ')
  ) task;
  String.split_on_char ' ' (Buffer.contents b)
  |> List.filter (fun w -> String.length w >= 2 && not (List.mem w stopwords))
  |> List.sort_uniq compare

let command_set files task =
  let arr, _, _, edges, scores, _ = build_graph files in
  let n = Array.length arr in
  let terms = terms_of task in
  let hits = Array.make n 0 in
  Array.iteri (fun i f ->
    let path = String.lowercase_ascii f.Index.path in
    List.iter (fun t ->
      if Lex.contains path t then hits.(i) <- hits.(i) + 1;
      List.iter (fun s ->
        if Lex.contains (String.lowercase_ascii s.Lex.name) t then hits.(i) <- hits.(i) + 3
      ) f.Index.symbols
    ) terms
  ) arr;
  let seed_pick =
    List.init n Fun.id
    |> List.filter (fun i -> hits.(i) > 0)
    |> List.sort (fun a b ->
      let c = compare hits.(b) hits.(a) in
      if c <> 0 then c else compare scores.(b) scores.(a))
    |> List.filteri (fun k _ -> k < 4)
  in
  if seed_pick = [] then begin
    if !json then emit (jobj [ "task", q task; "files", "[]"; "error", q "no file matches" ])
    else Printf.eprintf "no file matches: %s\n" task;
    1
  end else begin
    let dist = Array.make n (-1) and reason = Array.make n "" in
    let queue = Queue.create () in
    List.iter (fun i -> dist.(i) <- 0; reason.(i) <- "seed"; Queue.add i queue) seed_pick;
    Array.iter (fun e ->
      if List.mem e.Graph.dst seed_pick && dist.(e.Graph.src) < 0 then begin
        dist.(e.Graph.src) <- 1;
        reason.(e.Graph.src) <- "uses " ^ arr.(e.Graph.dst).Index.path;
        Queue.add e.Graph.src queue
      end
    ) edges;
    while not (Queue.is_empty queue) do
      let i = Queue.pop queue in
      if dist.(i) < 4 then
        Array.iter (fun e ->
          if e.Graph.src = i && dist.(e.Graph.dst) < 0 then begin
            dist.(e.Graph.dst) <- dist.(i) + 1;
            reason.(e.Graph.dst) <- "from " ^ arr.(i).Index.path;
            Queue.add e.Graph.dst queue
          end
        ) edges
    done;
    let members =
      List.init n Fun.id |> List.filter (fun i -> dist.(i) >= 0)
      |> List.sort (fun a b ->
        let c = compare dist.(a) dist.(b) in
        if c <> 0 then c else compare scores.(b) scores.(a))
    in
    let inside = Hashtbl.create 128 and used = ref 0 and shown = ref [] in
    List.iter (fun i ->
      let f = arr.(i) in
      let is_seed = dist.(i) = 0 in
      if is_seed || !used + f.Index.lines <= !budget then begin
        used := !used + f.Index.lines;
        Hashtbl.replace inside i ();
        shown := i :: !shown
      end
    ) members;
    let internal = ref 0 and boundary = ref 0 in
    Array.iter (fun e ->
      let a = Hashtbl.mem inside e.Graph.src and b = Hashtbl.mem inside e.Graph.dst in
      if a && b then incr internal else if a || b then incr boundary
    ) edges;
    let covered = !internal + !boundary in
    let pct = if covered = 0 then 100.0 else 100.0 *. float_of_int !internal /. float_of_int covered in
    let lines = List.fold_left (fun s i -> s + arr.(i).Index.lines) 0 !shown in
    let total_lines = Array.fold_left (fun s f -> s + f.Index.lines) 0 arr in
    if !json then
      emit (jobj [ "task", q task;
                   "files", jarr (List.map (fun i ->
                     jobj [ "path", q arr.(i).Index.path; "reason", q reason.(i);
                            "lines", jint arr.(i).Index.lines ]) (List.rev !shown));
                   "lines", jint lines;
                   "coverage", Printf.sprintf "%.1f" pct;
                   "inside", jint !internal; "leaving", jint !boundary;
                   "excluded_files", jint (n - List.length !shown);
                   "excluded_lines", jint (total_lines - !used) ])
    else begin
      Printf.printf "read set for \"%s\"\n\n" task;
      List.iter (fun i ->
        let f = arr.(i) in
        Printf.printf "  %-34s %s\n" f.Index.path reason.(i)
      ) (List.rev !shown);
      Printf.printf "\n%d files · %s lines\n"
        (List.length !shown) (thousands lines);
      Printf.printf "covers %.0f%% of the edge closure · %s inside · %s leaving\n"
        pct (thousands !internal) (thousands !boundary);
      Printf.printf "excluded %s files (%s lines)\n"
        (thousands (n - List.length !shown)) (thousands (total_lines - !used))
    end;
    0
  end

let command_check files =
  let arr, _, _, edges, _, _ = build_graph files in
  let cycles = Graph.cycles (Array.length arr) edges in
  let over =
    Array.to_list arr
    |> List.filter (fun f -> f.Index.lines > 2000)
    |> List.sort (fun a b -> compare b.Index.lines a.Index.lines)
  in
  let ok = cycles = [] && over = [] in
  if !json then begin
    emit (jobj [ "files", jint (Array.length arr); "edges", jint (Array.length edges);
                 "cycles", jarr (List.map (fun cycle ->
                   jarr (List.map (fun i -> q arr.(i).Index.path) cycle)) cycles);
                 "large", jarr (List.map (fun f ->
                   jobj [ "path", q f.Index.path; "lines", jint f.Index.lines ]) over);
                 "ok", if ok then "true" else "false" ]);
    if ok then 0 else 1
  end else begin
    Printf.printf "checked %s files · %s edges\n\n"
      (thousands (Array.length arr)) (thousands (Array.length edges));
    if ok then begin
      Printf.printf "ok: no cycles, no oversized files\n";
      0
    end else begin
      List.iter (fun cycle ->
        Printf.printf "cycle  %d files\n" (List.length cycle);
        List.iter (fun i -> Printf.printf "         %s\n" arr.(i).Index.path) cycle
      ) (List.filteri (fun k _ -> k < 5) cycles);
      if List.length cycles > 5 then Printf.printf "       ... %d more cycles\n" (List.length cycles - 5);
      List.iter (fun f ->
        Printf.printf "large  %s  %s lines\n" f.Index.path (thousands f.Index.lines)
      ) over;
      if cycles <> [] then Printf.printf "\n%d import cycles\n" (List.length cycles);
      if over <> [] then Printf.printf "%d files over 2,000 lines\n" (List.length over);
      1
    end
  end

(* --- tags --------------------------------------------------------------- *)

let tag_kind = function
  | "function" -> "f" | "method" -> "m" | "class" -> "c" | "interface" -> "i"
  | "trait" -> "i" | "type" -> "t" | "enum" -> "e" | "struct" -> "s"
  | "module" -> "n" | "package" -> "n" | "table" -> "t" | "view" -> "v"
  | _ -> "v"

let command_tags files out =
  let rows = ref [] in
  List.iter (fun f ->
    List.iter (fun s ->
      rows := Printf.sprintf "%s\t%s\t%d;\"\t%s"
        s.Lex.name f.Index.path s.Lex.line (tag_kind s.Lex.kind) :: !rows
    ) f.Index.symbols
  ) files;
  let body = String.concat "\n" (List.sort compare !rows) ^ "\n" in
  (match out with
   | Some path -> Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc body)
   | None -> print_string body);
  0

(* --- exports and watch -------------------------------------------------- *)

let xml_esc s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '&' -> Buffer.add_string b "&amp;"
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let command_html files out =
  let arr, _, _, edges, scores, _ = build_graph files in
  let n = Array.length arr in
  let max_score = Array.fold_left max 0.0001 scores in
  let max_lines = Array.fold_left (fun m f -> max m f.Index.lines) 1 arr in
  let pos i =
    let a = 2.0 *. Float.pi *. float_of_int i /. float_of_int (max 1 n) in
    (420.0 +. 300.0 *. cos a, 340.0 +. 260.0 *. sin a)
  in
  let b = Buffer.create 65536 in
  Buffer.add_string b "<!doctype html><meta charset=\"utf-8\"><title>repowatch</title>\n";
  Buffer.add_string b "<style>body{background:#0f1115;color:#d8dee9;font:14px ui-monospace,monospace;margin:0}\
svg{display:block;margin:auto}line{stroke:#3b4252;stroke-width:.7}circle{stroke:#0f1115;stroke-width:1}\
text{fill:#d8dee9;font-size:10px;pointer-events:none}.dim{opacity:.12}</style>\n";
  Buffer.add_string b (Printf.sprintf "<h3 style=\"margin:12px 16px\">%s  ·  %d files  ·  %d edges</h3>\n"
    (xml_esc !root) n (Array.length edges));
  Buffer.add_string b (Printf.sprintf "<svg id=\"g\" width=\"840\" height=\"680\" viewBox=\"0 0 840 680\">\n");
  Array.iter (fun e ->
    let x1, y1 = pos e.Graph.src and x2, y2 = pos e.Graph.dst in
    Buffer.add_string b (Printf.sprintf "<line data-a=\"n%d\" data-b=\"n%d\" x1=\"%.1f\" y1=\"%.1f\" x2=\"%.1f\" y2=\"%.1f\"/>\n"
      e.Graph.src e.Graph.dst x1 y1 x2 y2)
  ) edges;
  Array.iteri (fun i f ->
    let x, y = pos i in
    let radius = 3.0 +. 9.0 *. float_of_int f.Index.lines /. float_of_int max_lines in
    let hue = 200.0 +. 120.0 *. (scores.(i) /. max_score) in
    Buffer.add_string b (Printf.sprintf "<circle id=\"n%d\" cx=\"%.1f\" cy=\"%.1f\" r=\"%.1f\" fill=\"hsl(%.0f,60%%,60%%)\"><title>%s  %.3f  %d lines</title></circle>\n"
      i x y radius hue (xml_esc f.Index.path) scores.(i) f.Index.lines)
  ) arr;
  let tops =
    List.init n Fun.id
    |> List.sort (fun a b -> compare scores.(b) scores.(a))
    |> List.filteri (fun k _ -> k < 12)
  in
  List.iter (fun i ->
    let x, y = pos i in
    Buffer.add_string b (Printf.sprintf "<text x=\"%.1f\" y=\"%.1f\">%s</text>\n"
      (x +. 8.0) (y +. 3.0) (xml_esc (Filename.basename arr.(i).Index.path)))
  ) tops;
  Buffer.add_string b "</svg>\n<script>\n\
const g=document.getElementById('g');\n\
g.addEventListener('click',e=>{const t=e.target;if(!t.id||t.tagName!=='circle')return;\n\
const on=new Set([t.id]);document.querySelectorAll('line').forEach(l=>{\n\
if(l.dataset.a===t.id)on.add(l.dataset.b);if(l.dataset.b===t.id)on.add(l.dataset.a)});\n\
document.querySelectorAll('circle').forEach(c=>c.classList.toggle('dim',!on.has(c.id)));});\n\
</script>\n";
  let path = match out with Some p -> p | None -> "graph.html" in
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc (Buffer.contents b));
  Printf.printf "wrote %s · %d nodes · %d edges\n" path n (Array.length edges);
  0

let rec mkdir_p p =
  if p <> "" && p <> "." && p <> "/" && not (Sys.file_exists p) then begin
    mkdir_p (Filename.dirname p);
    try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let command_wiki files out =
  let dir = match out with Some d -> d | None -> "repowatch-wiki" in
  mkdir_p dir;
  let arr, _, _, edges, scores, _ = build_graph files in
  let n = Array.length arr in
  let order = List.init n Fun.id
    |> List.sort (fun a b -> compare scores.(b) scores.(a)) in
  let by_path = Hashtbl.create n in
  Array.iteri (fun i f -> Hashtbl.replace by_path f.Index.path i) arr;
  let page p = p ^ ".md" in
  Array.iteri (fun i f ->
    let deps = Array.to_list edges
      |> List.filter (fun e -> e.Graph.src = i)
      |> List.map (fun e -> arr.(e.Graph.dst).Index.path) |> List.sort_uniq compare in
    let users = Array.to_list edges
      |> List.filter (fun e -> e.Graph.dst = i)
      |> List.map (fun e -> arr.(e.Graph.src).Index.path) |> List.sort_uniq compare in
    let b = Buffer.create 1024 in
    Buffer.add_string b (Printf.sprintf "# %s\n\nscore %.3f · %d lines · %d symbols\n\n" f.Index.path scores.(i) f.Index.lines (List.length f.Index.symbols));
    if f.Index.symbols <> [] then begin
      Buffer.add_string b "## symbols\n\n";
      List.iter (fun s -> Buffer.add_string b (Printf.sprintf "- `%s` %s line %d\n" s.Lex.kind s.Lex.name s.Lex.line)) f.Index.symbols;
      Buffer.add_char b '\n'
    end;
    if deps <> [] then begin
      Buffer.add_string b "## depends on\n\n";
      List.iter (fun p -> Buffer.add_string b (Printf.sprintf "- [[%s]]\n" p)) deps;
      Buffer.add_char b '\n'
    end;
    if users <> [] then begin
      Buffer.add_string b "## used by\n\n";
      List.iter (fun p -> Buffer.add_string b (Printf.sprintf "- [[%s]]\n" p)) users;
      Buffer.add_char b '\n'
    end;
    let path = Filename.concat dir (page f.Index.path) in
    let sub = Filename.dirname path in
    if sub <> dir && not (Sys.file_exists sub) then mkdir_p sub;
    Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc (Buffer.contents b))
  ) arr;
  let b = Buffer.create 4096 in
  Buffer.add_string b "# repowatch index\n\n";
  List.iter (fun i ->
    let f = arr.(i) in
    Buffer.add_string b (Printf.sprintf "- [[%s]] %.3f\n" f.Index.path scores.(i))
  ) (List.filteri (fun k _ -> k < 50) order);
  Out_channel.with_open_bin (Filename.concat dir "index.md")
    (fun oc -> Out_channel.output_string oc (Buffer.contents b));
  Printf.printf "wrote %s · %d pages\n" dir n;
  ignore by_path;
  0

let fingerprint root =
  Walk.walk root
  |> List.map (fun (p, st) -> Printf.sprintf "%s:%d:%.0f" p st.Unix.st_size st.Unix.st_mtime)
  |> String.concat "|"

let command_watch interval =
  let last = ref (fingerprint !root) in
  ignore (build !root);
  Printf.printf "watching %s\n%!" !root;
  while true do
    Unix.sleepf interval;
    let now = fingerprint !root in
    if now <> !last then begin
      last := now;
      let files = build !root in
      Printf.printf "%d files  %d symbols\n%!"
        (List.length files)
        (List.fold_left (fun s f -> s + List.length f.Index.symbols) 0 files)
    end
  done

(* --- snapshots and drift ------------------------------------------------ *)

let snapshot_path root name = Filename.concat (Index.dir root) ("snap-" ^ name)

let ranked files =
  let arr, _, _, _, scores, _ = build_graph files in
  let order =
    List.init (Array.length arr) Fun.id
    |> List.sort (fun a b ->
      let c = compare scores.(b) scores.(a) in
      if c <> 0 then c else compare arr.(a).Index.path arr.(b).Index.path)
  in
  (arr, scores, order)

let command_snapshot files name =
  let arr, scores, order = ranked files in
  let b = Buffer.create 4096 in
  List.iteri (fun rank i ->
    let f = arr.(i) in
    Buffer.add_string b
      (Printf.sprintf "%s\t%d\t%d\t%.6f\t%d\n"
         f.Index.path f.Index.lines (List.length f.Index.symbols) scores.(i) (rank + 1))
  ) order;
  let d = Index.dir !root in
  if not (Sys.file_exists d) then Unix.mkdir d 0o755;
  Out_channel.with_open_bin (snapshot_path !root name)
    (fun oc -> Out_channel.output_string oc (Buffer.contents b));
  if !json then
    emit (jobj [ "snapshot", q name; "files", jint (Array.length arr);
                 "lines", jint (Array.fold_left (fun s f -> s + f.Index.lines) 0 arr) ])
  else
    Printf.printf "saved %s · %s files · %s lines\n"
      name (thousands (Array.length arr))
      (thousands (Array.fold_left (fun s f -> s + f.Index.lines) 0 arr));
  0

type snap_row = { lines : int; symbols : int; score : float; rank : int }

let load_snapshot root name =
  let p = snapshot_path root name in
  if not (Sys.file_exists p) then None
  else
    let rows = Hashtbl.create 256 in
    In_channel.with_open_bin p In_channel.input_all
    |> String.split_on_char '\n'
    |> List.iter (fun line ->
      match String.split_on_char '\t' line with
      | [ path; lines; symbols; score; rank ] ->
        Hashtbl.replace rows path
          { lines = int_of_string lines; symbols = int_of_string symbols;
            score = float_of_string score; rank = int_of_string rank }
      | _ -> ());
    Some rows

let latest_snapshot root =
  let d = Index.dir root in
  if not (Sys.file_exists d) then None
  else
    Sys.readdir d |> Array.to_list
    |> List.filter (fun n -> Lex.starts n "snap-")
    |> List.sort (fun a b ->
      compare (Unix.stat (Filename.concat d b)).Unix.st_mtime
              (Unix.stat (Filename.concat d a)).Unix.st_mtime)
    |> function [] -> None | n :: _ -> Some (String.sub n 5 (String.length n - 5))

let command_drift files requested =
  match (match requested with Some n -> Some n | None -> latest_snapshot !root) with
  | None -> Printf.eprintf "no snapshot yet: repowatch snapshot NAME\n"; 1
  | Some name ->
    (match load_snapshot !root name with
     | None -> Printf.eprintf "no snapshot named %s\n" name; 1
     | Some before ->
       let arr, scores, order = ranked files in
       let rank_now = Hashtbl.create 256 in
       List.iteri (fun r i -> Hashtbl.replace rank_now arr.(i).Index.path (r + 1)) order;
       let added = ref [] and removed = ref [] and moved = ref [] in
       Array.iter (fun f ->
         match Hashtbl.find_opt before f.Index.path with
         | None -> added := f.Index.path :: !added
         | Some old ->
           let rank = Option.value ~default:0 (Hashtbl.find_opt rank_now f.Index.path) in
           if old.rank <> rank then
             moved := (f.Index.path, old.rank, rank) :: !moved
       ) arr;
       Hashtbl.iter (fun path _ ->
         if not (Array.exists (fun f -> f.Index.path = path) arr) then
           removed := path :: !removed
       ) before;
       let now_lines = Array.fold_left (fun s f -> s + f.Index.lines) 0 arr in
       let old_lines = Hashtbl.fold (fun _ r s -> s + r.lines) before 0 in
       if !json then begin
         emit (jobj [ "snapshot", q name;
                      "files_before", jint (Hashtbl.length before);
                      "files_after", jint (Array.length arr);
                      "lines_before", jint old_lines; "lines_after", jint now_lines;
                      "added", jarr (List.map q (List.sort compare !added));
                      "removed", jarr (List.map q (List.sort compare !removed));
                      "moved", jarr (List.map (fun (p, a, b) ->
                        jobj [ "path", q p; "before", jint a; "after", jint b ]) !moved) ])
       end else begin
       Printf.printf "since %s\n\n" name;
       Printf.printf "  files   %d -> %d\n" (Hashtbl.length before) (Array.length arr);
       Printf.printf "  lines  %s -> %s (%+d)\n"
         (thousands old_lines) (thousands now_lines) (now_lines - old_lines);
       if !added <> [] then begin
         Printf.printf "\nadded (%d)\n" (List.length !added);
         List.sort compare !added |> List.filteri (fun k _ -> k < 8)
         |> List.iter (fun p -> Printf.printf "  + %s\n" p)
       end;
       if !removed <> [] then begin
         Printf.printf "\nremoved (%d)\n" (List.length !removed);
         List.sort compare !removed |> List.filteri (fun k _ -> k < 8)
         |> List.iter (fun p -> Printf.printf "  - %s\n" p)
       end;
       let moves =
         List.sort (fun (_, a1, b1) (_, a2, b2) ->
           compare (abs (a2 - b2)) (abs (a1 - b1))) !moved
         |> List.filteri (fun k _ -> k < 8)
       in
       if moves <> [] then begin
         Printf.printf "\nrank moves\n";
         List.iter (fun (p, a, b) ->
           Printf.printf "  %s  %d -> %d\n" p a b) moves
       end;
       end;
       0)

(* --- entry -------------------------------------------------------------- *)

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  (match arg_value "--root" "" with "" -> () | r -> root := r);
  (match arg_value "--budget" "" with "" -> () | r -> (match int_of_string_opt r with Some n -> budget := n | None -> ()));
  if List.mem "--json" args then json := true;
  let cmd = match args with c :: _ when String.length c > 0 && c.[0] <> '-' -> c | _ -> "map" in
  let rec plain acc = function
    | [] -> List.rev acc
    | ("--root" | "--budget") :: _ :: rest -> plain acc rest
    | a :: rest when String.length a > 0 && a.[0] = '-' -> plain acc rest
    | a :: rest -> plain (a :: acc) rest
  in
  let positional = plain [] args in
  let files = if cmd = "version" || cmd = "lsp" then [] else build !root in
  let code =
    match cmd with
    | "version" -> print_endline version; 0
    | "map" -> command_map files; 0
    | "stats" -> command_stats files; 0
    | "refs" -> (match List.nth_opt positional 1 with Some n -> command_refs files n | None -> 2)
    | "why" -> (match List.nth_opt positional 1 with Some p -> command_why files p | None -> 2)
    | "set" -> command_set files (String.concat " " (List.tl positional))
    | "check" -> command_check files
    | "snapshot" -> (match List.nth_opt positional 1 with Some n -> command_snapshot files n | None -> 2)
    | "drift" -> command_drift files (List.nth_opt positional 1)
    | "tags" -> command_tags files (match arg_value "--out" "" with "" -> None | p -> Some p)
    | "graph" -> command_html files (match arg_value "--out" "" with "" -> None | p -> Some p)
    | "wiki" -> command_wiki files (match arg_value "--out" "" with "" -> None | p -> Some p)
    | "watch" ->
      (match Float.of_string_opt (arg_value "--interval" "") with
       | Some s -> command_watch s
       | None -> command_watch 1.0)
    | "lsp" -> serve_lsp ()
    | "help" | "--help" -> print_string usage; 0
    | _ -> Printf.eprintf "unknown command: %s\n%s" cmd usage; 2
  in
  exit code
