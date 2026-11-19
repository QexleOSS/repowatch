(* Index file: one text file under .repowatch/. Line-oriented, tab separated,
   readable with cat. Nothing native, nothing binary. *)

type file = {
  path : string;
  lang : string;
  size : int;
  mtime : float;
  lines : int;
  hash : string;
  imports : string list;
  calls : string list;
  symbols : Lex.symbol list;
}

let sep = '\x1f'
let sym_sep = '\x1e'

let join = String.concat (String.make 1 sep)
let split s = if s = "" then [] else String.split_on_char sep s

let symbol_to_string s =
  String.concat ":" [ s.Lex.name; s.Lex.kind; string_of_int s.Lex.line; if s.Lex.exported then "1" else "0" ]

let symbol_of_string s =
  match String.split_on_char ':' s with
  | [ name; kind; line; exp ] ->
    Some { Lex.name; kind; line = int_of_string_opt line |> Option.value ~default:0;
           exported = exp = "1" }
  | _ -> None

let line_of_file f =
  String.concat "\t" [
    "F"; f.path; f.lang; string_of_int f.size; Printf.sprintf "%.0f" f.mtime;
    string_of_int f.lines; f.hash;
    join f.imports; join f.calls;
    String.concat (String.make 1 sym_sep) (List.map symbol_to_string f.symbols);
  ]

let file_of_line line =
  match String.split_on_char '\t' line with
  | [ _; path; lang; size; mtime; lines; hash; imports; calls; symbols ] ->
    Some {
      path; lang; size = int_of_string size; mtime = float_of_string mtime;
      lines = int_of_string lines; hash;
      imports = split imports; calls = split calls;
      symbols = List.filter_map symbol_of_string (String.split_on_char sym_sep symbols);
    }
  | _ -> None

let dir root = Filename.concat root ".repowatch"
let path root = Filename.concat (dir root) "index"

(* Bump when extraction changes: an old index must never survive a new lexer. *)
let format = "3"

let load root =
  let p = path root in
  if not (Sys.file_exists p) then []
  else
    match In_channel.with_open_bin p In_channel.input_all |> String.split_on_char '\n' with
    | first :: rest when first = format -> List.filter_map file_of_line rest
    | _ -> []

let save root files =
  let d = dir root in
  if not (Sys.file_exists d) then Unix.mkdir d 0o755;
  let b = Buffer.create (1 lsl 20) in
  Buffer.add_string b format;
  Buffer.add_char b '\n';
  List.iter (fun f -> Buffer.add_string b (line_of_file f); Buffer.add_char b '\n') files;
  Out_channel.with_open_bin (path root) (fun oc -> Out_channel.output_string oc (Buffer.contents b))
