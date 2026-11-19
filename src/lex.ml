(* Lexical extraction. No parser generator, no C: a state machine that knows
   where strings and comments are, then per-language line patterns on top.

   shortcut: comments are blanked, not parsed; a file whose block comment
   spans a string literal can still yield a wrong import. Upgrade only if a
   real codebase shows it. *)

type style = C | Hash | Ada | Ocaml | Haskell

type symbol = { name : string; kind : string; line : int; exported : bool }

let find_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  if m = 0 then Some 0 else go 0

let contains s sub = find_sub s sub <> None

let starts s prefix =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* Character-position prefix check. Allocating the tail here made every file
   quadratic; this copies only the prefix length. *)
let at s i lit =
  let m = String.length lit in
  i + m <= String.length s && String.sub s i m = lit

let words s =
  String.split_on_char ' ' s |> List.filter (fun w -> w <> "")

let quoted s =
  let out = ref [] and i = ref 0 and n = String.length s in
  while !i < n do
    (match s.[!i] with
     | '"' | '\'' | '`' as q ->
       let j = ref (!i + 1) in
       while !j < n && s.[!j] <> q do incr j done;
       if !j < n then out := String.sub s (!i + 1) (!j - !i - 1) :: !out;
       i := !j + 1
     | _ -> incr i)
  done;
  List.rev !out

(* Blank comments, keep strings and newlines. *)
let strip_comments style content =
  let b = Buffer.create (String.length content) in
  let n = String.length content in
  let block_end =
    match style with C -> "*/" | Ocaml -> "*)" | Haskell -> "-}" | _ -> ""
  in
  let block_open =
    match style with C -> "/*" | Ocaml -> "(*" | Haskell -> "{-" | _ -> ""
  in
  let line_marker =
    match style with
    | C | Ocaml -> None
    | Hash -> Some "#"
    | Ada -> Some "--"
    | Haskell -> Some "--"
  in
  let rec normal i =
    if i >= n then ()
    else if block_open <> "" && at content i block_open then begin
      Buffer.add_char b ' '; block i (i + String.length block_open)
    end
    else match line_marker with
      | Some m when at content i m -> line_comment i
      | _ ->
        (match content.[i] with
         | '"' | '\'' | '`' as q ->
           Buffer.add_char b q; string_lit (i + 1) q
         | c -> Buffer.add_char b c; normal (i + 1))
  and block i j =
    if j >= n then ()
    else if at content j block_end then begin
      Buffer.add_char b ' '; normal (j + String.length block_end)
    end
    else begin
      if content.[j] = '\n' then Buffer.add_char b '\n' else Buffer.add_char b ' ';
      block i (j + 1)
    end
  and line_comment i =
    if i >= n then ()
    else if content.[i] = '\n' then begin Buffer.add_char b '\n'; normal (i + 1) end
    else begin Buffer.add_char b ' '; line_comment (i + 1) end
  and string_lit i q =
    if i >= n then ()
    else if content.[i] = '\\' && i + 1 < n then begin
      Buffer.add_char b content.[i]; Buffer.add_char b content.[i + 1];
      string_lit (i + 2) q
    end
    else if content.[i] = q then begin Buffer.add_char b q; normal (i + 1) end
    else begin
      if content.[i] = '\n' then Buffer.add_char b '\n' else Buffer.add_char b content.[i];
      string_lit (i + 1) q
    end
  in
  normal 0;
  Buffer.contents b

let style_of_lang = function
  | "python" | "ruby" | "shell" | "lua" -> Hash
  | "ada" -> Ada
  | "ocaml" -> Ocaml
  | "haskell" -> Haskell
  | _ -> C

(* Remove string contents, keep the delimiters, for call scanning. *)
let blank_strings s =
  let b = Bytes.of_string s and n = String.length s and i = ref 0 in
  let q = ref None in
  while !i < n do
    (match !q with
     | Some c ->
       if Bytes.get b !i = '\\' then begin
         Bytes.set b !i ' '; if !i + 1 < n then Bytes.set b (!i + 1) ' '; i := !i + 2
       end else if Bytes.get b !i = c then begin q := None; incr i end
       else begin if Bytes.get b !i <> '\n' then Bytes.set b !i ' '; incr i end
     | None ->
       (match Bytes.get b !i with
        | '"' | '\'' | '`' as c -> q := Some c; incr i
        | _ -> incr i))
  done;
  Bytes.to_string b

let import_specs lang line =
  let t = String.trim line in
  let str_specs () = quoted t in
  match lang with
  | "typescript" | "tsx" | "javascript" ->
    if starts t "import " || starts t "export " && contains t " from "
       || contains t "require(" || (starts t "\"" || starts t "'")
    then str_specs () else []
  | "go" ->
    if starts t "import " || (String.length t > 1 && t.[0] = '"') then str_specs () else []
  | "python" ->
    if starts t "from " then
      (match words t with _ :: modl :: _ -> [ modl ] | _ -> [])
    else if starts t "import " then
      String.sub t 7 (String.length t - 7)
      |> String.split_on_char ','
      |> List.map (fun w -> String.trim (List.hd (words (String.trim w))))
    else []
  | "rust" ->
    if starts t "mod " then [ String.sub t 4 (String.length t - 4) |> String.trim ]
    else if starts t "use " then
      let body = String.sub t 4 (String.length t - 4) in
      let cut = match find_sub body "{" with Some i -> i | None -> String.length body in
      let body = String.sub body 0 cut in
      [ String.map (function ';' -> ' ' | c -> c) body |> String.trim ]
    else []
  | "java" | "c_sharp" ->
    if starts t "import " || starts t "using " then
      let body = if starts t "import " then String.sub t 7 (String.length t - 7)
        else String.sub t 6 (String.length t - 6) in
      [ String.map (function ';' -> ' ' | c -> c) body |> String.trim ]
    else []
  | "ruby" ->
    if starts t "require_relative " then str_specs ()
    else if starts t "require " then str_specs ()
    else []
  | "php" ->
    if starts t "use " then
      [ String.sub t 4 (String.length t - 4)
        |> String.map (function ';' -> ' ' | c -> c) |> String.trim ]
    else []
  | "lua" ->
    if contains t "require" then str_specs () else []
  | "haskell" ->
    if starts t "import " then
      let body = String.sub t 7 (String.length t - 7) |> String.trim in
      (match words body with
       | w :: _ when w <> "qualified" -> [ w ]
       | _ :: w :: _ -> [ w ]
       | _ -> [])
    else []
  | "zig" ->
    if contains t "@import(" then str_specs () else []
  | "kotlin" | "scala" | "julia" | "swift" | "elixir" | "clojure" ->
    let keywords = [ "import "; "using "; "alias "; "require "; "include " ] in
    if List.exists (fun k -> starts t k) keywords then
      (match words t with
       | _ :: w :: _ ->
         let w = String.map (fun c -> if c = ';' || c = '_' then ' ' else c) w in
         [ String.trim w ]
       | _ -> [])
    else []
  | "dart" ->
    if starts t "import " || starts t "export " then str_specs () else []
  | "erlang" ->
    if starts t "-include(" then str_specs () else []
  | "r" ->
    if starts t "library(" || starts t "require(" || starts t "source(" then
      (match quoted t with
       | s :: _ -> [ s ]
       | [] ->
         (match find_sub t "(" with
          | Some i -> [ String.sub t (i + 1) (String.length t - i - 2) |> String.trim ]
          | None -> []))
    else []
  | "objc" | "solidity" | "protobuf" ->
    if starts t "#import" || starts t "import " then str_specs () else []
  | "vue" | "svelte" ->
    if starts t "import " || contains t "require(" then str_specs () else []
  | "nix" ->
    if starts t "import " then
      [ String.sub t 7 (String.length t - 7) |> String.trim ]
    else []
  | "perl" ->
    if starts t "use " then
      [ String.sub t 4 (String.length t - 4) |> String.map (fun c -> if c = ';' then ' ' else c) |> String.trim ]
    else if starts t "require " then str_specs ()
    else []
  | "fsharp" ->
    if starts t "open " then [ String.sub t 5 (String.length t - 5) |> String.trim ] else []
  | "groovy" ->
    if starts t "import " then
      let body = String.sub t 7 (String.length t - 7) in
      let body = if starts (String.trim body) "static " then String.sub (String.trim body) 7 (String.length (String.trim body) - 7) else body in
      [ String.map (fun c -> if c = ';' then ' ' else c) body |> String.trim ]
    else []
  | "pascal" ->
    if starts (String.lowercase_ascii t) "uses " then
      [ String.sub t 5 (String.length t - 5) |>
        String.map (fun c -> if c = ',' || c = ';' then ' ' else c) |> String.trim ]
    else []
  | "fortran" ->
    if starts (String.lowercase_ascii t) "use " then
      (match words (String.sub t 4 (String.length t - 4)) with
       | w :: _ -> [ String.map (fun c -> if c = ',' then ' ' else c) w |> String.trim ]
       | [] -> [])
    else []
  | "graphql" ->
    if starts t "#import" then str_specs () else []
  | "nim" | "crystal" ->
    if starts t "import " || starts t "require " then str_specs () else []
  | "d" ->
    if starts t "import " then
      [ String.sub t 7 (String.length t - 7) |>
        String.map (fun c -> if c = ';' || c = ',' then ' ' else c) |> String.trim ]
    else []
  | "verilog" ->
    if starts t "`include" then str_specs () else []
  | "vhdl" ->
    let low = String.lowercase_ascii t in
    if starts low "library " || starts low "use " then
      (match words t with
       | _ :: w :: _ -> [ String.map (fun c -> if c = ';' || c = ',' then ' ' else c) w |> String.trim ]
       | _ -> [])
    else []
  | "starlark" ->
    if starts t "load(" then str_specs () else []
  | "cmake" -> []
  | "vb" ->
    let low = String.lowercase_ascii t in
    if starts low "imports " then
      [ String.sub t 8 (String.length t - 8) |> String.trim ] else []
  | "asm" ->
    if starts t ".include" then str_specs () else []
  | "prolog" ->
    (match find_sub t "library(" with
     | Some i ->
       let rest = String.sub t (i + 8) (String.length t - i - 8) in
       (match find_sub rest ")" with
        | Some j -> [ String.sub rest 0 j ]
        | None -> [])
     | None -> str_specs ())
  | "lisp" ->
    if contains t "(load " || contains t "(require " then str_specs ()
    else []
  | "msbuild" | "xaml" ->
    if contains t "Include=" || contains t "Class=" || contains t "<Target" then str_specs () else []
  | "sln" ->
    let quoted = quoted t in
    List.filter (fun q ->
      List.exists (fun e -> Filename.check_suffix q e)
        [ ".csproj"; ".fsproj"; ".vbproj"; ".sln" ]) quoted
  | "razor" ->
    let low = String.lowercase_ascii t in
    if starts low "@using " || starts low "@model " then
      [ String.sub t 7 (String.length t - 7) |> String.map (fun c -> if c = ';' then ' ' else c) |> String.trim ]
    else []
  | "bash" | "shell" ->
    if starts t "source " then
      [ String.sub t 7 (String.length t - 7) |> String.trim ]
    else if starts t ". " then
      [ String.sub t 2 (String.length t - 2) |> String.trim ]
    else []
  | "c" | "cpp" ->
    if starts t "#include" then
      let rest = String.sub t 8 (String.length t - 8) |> String.trim in
      if rest <> "" && (rest.[0] = '"' || rest.[0] = '<') then
        [ String.sub rest 1 (String.length rest - 2) ]
      else []
    else []
  | "ocaml" ->
    if starts t "open " then [ String.sub t 5 (String.length t - 5) |> String.trim ]
    else if starts t "include " then [ String.sub t 8 (String.length t - 8) |> String.trim ]
    else []
  | "ada" ->
    if starts t "with " then [ String.sub t 5 (String.length t - 5) |> String.trim ]
    else if starts t "use " then [ String.sub t 4 (String.length t - 4) |> String.trim ]
    else []
  | "elm" ->
    if starts t "import " then
      (match words (String.sub t 7 (String.length t - 7)) with
       | w :: _ -> [ w ]
       | [] -> [])
    else []
  | "vala" ->
    if starts t "using " then
      [ String.sub t 6 (String.length t - 6) |> String.map (fun c -> if c = ';' then ' ' else c) |> String.trim ]
    else []
  | "cobol" ->
    if starts (String.lowercase_ascii t) "copy " then str_specs () else []
  | "abap" ->
    if starts (String.lowercase_ascii t) "include " then
      [ String.sub t 8 (String.length t - 8) |> String.map (fun c -> if c = '.' then ' ' else c) |> String.trim ]
    else []
  | "glsl" | "hlsl" | "opencl" ->
    if starts t "#include" then str_specs () else []
  | _ -> []

(* Capitalised module references: Lex.extract, Filename.concat, ...
   Stdlib names simply fail to resolve and cost nothing. *)
let module_refs content =
  let n = String.length content and out = Hashtbl.create 16 in
  let is_id c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                || (c >= '0' && c <= '9') || c = '_' || c = '\'' in
  let rec go i =
    if i >= n then ()
    else if content.[i] >= 'A' && content.[i] <= 'Z' then begin
      let j = ref i in
      while !j < n && is_id content.[!j] do incr j done;
      if !j < n && content.[!j] = '.' then
        Hashtbl.replace out (String.sub content i (!j - i)) ();
      go (max (!j) (i + 1))
    end else go (i + 1)
  in
  go 0;
  Hashtbl.fold (fun k () acc -> k :: acc) out []

let symbol_of_line lang line_no line =
  let t = String.trim line in
  let lower = String.lowercase_ascii t in
  let exported = not (starts t "private " || starts t "protected " || starts t "static ") in
  let take_from prefix kind =
    if starts t prefix then
      let rest = String.sub t (String.length prefix) (String.length t - String.length prefix) in
      match words rest with
      | w :: _ ->
        let name = String.map (fun c -> if c = '(' || c = ')' || c = ':' || c = '<' || c = '=' || c = '{' || c = '}' || c = ',' || c = ';' || c = '[' || c = ']' then ' ' else c) w in
        (match words name with
         | n :: _ when n <> "" -> Some { name = n; kind; line = line_no; exported }
         | _ -> None)
      | [] -> None
    else None
  in
  let candidates =
    match lang with
    | "typescript" | "tsx" | "javascript" ->
      [ "export default function ", "function"; "export async function ", "function";
        "export function ", "function"; "async function ", "function"; "function ", "function";
        "export class ", "class"; "class ", "class"; "export interface ", "interface";
        "interface ", "interface"; "export type ", "type"; "type ", "type";
        "export enum ", "enum"; "enum ", "enum"; "export const ", "const"; "const ", "const" ]
    | "python" ->
      let base = List.filter (fun (p, _) -> starts t p) [ ("def ", "function"); ("class ", "class"); ("async def ", "function") ] in
      base
    | "rust" ->
      [ "pub async fn ", "function"; "pub fn ", "function"; "async fn ", "function";
        "fn ", "function"; "pub struct ", "struct"; "struct ", "struct";
        "pub enum ", "enum"; "enum ", "enum"; "pub trait ", "trait"; "trait ", "trait";
        "impl ", "impl" ]
    | "go" ->
      [ "func (", "method"; "func ", "function"; "type ", "type" ]
    | "java" | "c_sharp" ->
      [ "public class ", "class"; "class ", "class"; "interface ", "interface";
        "enum ", "enum"; "public ", "method"; "private ", "method"; "protected ", "method" ]
    | "c" | "cpp" ->
      [ "struct ", "struct"; "class ", "class"; "typedef ", "type"; "#define ", "const" ]
    | "ada" ->
      [ "procedure ", "function"; "function ", "function"; "package ", "module"; "type ", "type" ]
    | "ocaml" ->
      [ "let rec ", "function"; "let ", "function"; "type ", "type"; "module ", "module" ]
    | "zig" ->
      [ "pub fn ", "function"; "fn ", "function"; "pub const ", "const"; "const ", "const" ]
    | "ruby" ->
      [ "def self.", "function"; "def ", "function"; "class ", "class"; "module ", "module" ]
    | "php" ->
      [ "function ", "function"; "class ", "class"; "interface ", "interface"; "trait ", "trait" ]
    | "lua" ->
      [ "local function ", "function"; "function ", "function" ]
    | "haskell" ->
      [ "data ", "type"; "newtype ", "type"; "class ", "class" ]
    | "kotlin" ->
      [ "data class ", "class"; "fun ", "function"; "class ", "class";
        "object ", "module"; "interface ", "interface" ]
    | "scala" ->
      [ "case class ", "class"; "def ", "function"; "class ", "class";
        "object ", "module"; "trait ", "trait" ]
    | "swift" ->
      [ "func ", "function"; "class ", "class"; "struct ", "struct";
        "protocol ", "interface"; "enum ", "enum" ]
    | "dart" ->
      [ "class ", "class"; "mixin ", "class"; "enum ", "enum"; "extension ", "module" ]
    | "elixir" ->
      [ "defmodule ", "module"; "defp ", "function"; "def ", "function" ]
    | "erlang" ->
      [ "-module(", "module" ]
    | "clojure" ->
      [ "(defn ", "function"; "(defmacro ", "function"; "(def ", "const"; "(ns ", "module" ]
    | "julia" ->
      [ "function ", "function"; "struct ", "struct"; "module ", "module" ]
    | "objc" ->
      [ "@interface ", "interface"; "@implementation ", "class"; "@protocol ", "interface" ]
    | "solidity" ->
      [ "contract ", "class"; "library ", "module"; "interface ", "interface";
        "function ", "function"; "event ", "field" ]
    | "protobuf" ->
      [ "message ", "struct"; "service ", "interface"; "rpc ", "function"; "enum ", "enum" ]
    | "vue" | "svelte" ->
      [ "export default function ", "function"; "export function ", "function";
        "function ", "function"; "export class ", "class"; "class ", "class";
        "export const ", "const"; "const ", "const" ]
    | "perl" ->
      [ "sub ", "function" ]
    | "fsharp" ->
      [ "let ", "function"; "member ", "method"; "type ", "type"; "module ", "module" ]
    | "groovy" ->
      [ "def ", "function"; "class ", "class"; "interface ", "interface"; "trait ", "trait" ]
    | "nim" ->
      [ "proc ", "function"; "func ", "function"; "type ", "type" ]
    | "crystal" ->
      [ "def ", "function"; "class ", "class"; "module ", "module" ]
    | "d" ->
      [ "class ", "class"; "struct ", "struct"; "interface ", "interface" ]
    | "elm" ->
      [ "export type ", "type"; "type ", "type"; "export port ", "function";
        "port ", "function"; "module ", "module" ]
    | "vala" ->
      [ "class ", "class"; "interface ", "interface"; "struct ", "struct"; "enum ", "enum" ]
    | "apex" ->
      [ "class ", "class"; "interface ", "interface"; "trigger ", "trigger"; "enum ", "enum" ]
    | "bash" | "shell" ->
      [ "function ", "function" ]
    | _ -> []
  in
  let rec first = function
    | [] -> None
    | (prefix, kind) :: rest ->
      (match take_from prefix kind with Some s -> Some s | None -> first rest)
  in
  if lang = "sql" then begin
    let take prefix kind =
      if starts lower prefix then
        match words (String.sub lower (String.length prefix) (String.length lower - String.length prefix)) with
        | w :: _ ->
          let name = String.map (fun c -> if c = '(' || c = ';' then ' ' else c) w |> String.trim in
          (match words name with
           | n :: _ when n <> "" -> Some { name = n; kind; line = line_no; exported = true }
           | _ -> None)
        | [] -> None
      else None
    in
    let rec pick = function
      | [] -> None
      | (prefix, kind) :: rest ->
        (match take prefix kind with Some s -> Some s | None -> pick rest)
    in
    pick [ ("create table ", "table"); ("create view ", "view");
           ("create index ", "index"); ("create function ", "function") ]
  end
  else if lang = "nix" then
    (match find_sub t " = " with
     | Some i ->
       let lhs = String.trim (String.sub t 0 i) in
       (match words lhs with
        | [ n ] when n <> "" && n.[0] <> '{' && n.[0] <> '(' ->
          Some { name = n; kind = "const"; line = line_no; exported = true }
        | _ -> None)
     | None -> None)
  else if lang = "terraform" then
    (match quoted t with
     | [] -> None
     | names ->
       let tail = List.rev names in
       (match tail with
        | name :: _ -> Some { name; kind = "resource"; line = line_no; exported = true }
        | [] -> None))
  else if lang = "bash" || lang = "shell" then
    (match find_sub t "()" with
     | Some i ->
       let name = String.trim (String.sub t 0 i) in
       if name <> "" && not (String.contains name ' ') then
         Some { name; kind = "function"; line = line_no; exported = true }
       else first candidates
     | None -> first candidates)
  else if lang = "msbuild" then
    (match find_sub t "<Target Name=" with
     | Some i ->
       let rest = String.sub t (i + 13) (String.length t - i - 13) in
       (match quoted rest with
        | name :: _ -> Some { name; kind = "target"; line = line_no; exported = true }
        | [] -> None)
     | None -> None)
  else if lang = "asm" then
    (if starts t ".globl " || starts t ".global " then
       (match words t with _ :: n :: _ -> Some { name = n; kind = "function"; line = line_no; exported = true } | _ -> None)
     else if t <> "" && t.[String.length t - 1] = ':' && not (String.contains t ' ') then
       Some { name = String.sub t 0 (String.length t - 1); kind = "label"; line = line_no; exported = true }
     else None)
  else if lang = "lisp" then
    let pick = [ "(defun "; "(defmacro "; "(defvar "; "(define ("; "(define " ] in
    let rec go = function
      | [] -> None
      | p :: rest ->
        (match find_sub lower p with
         | Some i ->
           let rest_s = String.sub t (i + String.length p) (String.length t - i - String.length p) in
           (match words rest_s with
            | w :: _ ->
              let n = String.map (fun c -> if c = '(' || c = ')' then ' ' else c) w |> String.trim in
              if n = "" then None else Some { name = n; kind = "function"; line = line_no; exported = true }
            | [] -> None)
         | None -> go rest)
    in
    go pick
  else if lang = "prolog" then
    (match find_sub t ":-" with
     | Some i when i > 0 ->
       let head = String.sub t 0 i |> String.trim in
       let name = match find_sub head "(" with Some j -> String.sub head 0 j | None -> head in
       if name = "" then None else Some { name; kind = "rule"; line = line_no; exported = true }
     | _ -> None)
  else if lang = "fortran" || lang = "powershell" || lang = "pascal" || lang = "graphql"
          || lang = "verilog" || lang = "vhdl" || lang = "starlark" || lang = "cmake" || lang = "vb" then
    let pick =
      [ ("subroutine ", "function"); ("function ", "function");
        ("module ", "module"); ("program ", "module"); ("procedure ", "function");
        ("type ", "type"); ("input ", "type"); ("enum ", "enum");
        ("interface ", "interface"); ("query ", "function"); ("class ", "class");
        ("entity ", "module"); ("architecture ", "class"); ("package ", "module");
        ("def ", "function"); ("add_library(", "function");
        ("add_executable(", "function"); ("function(", "function");
        ("sub ", "method"); ("property ", "variable") ]
    in
    let rec go = function
      | [] -> None
      | (prefix, kind) :: rest ->
        if starts lower prefix then
          (match words (String.sub lower (String.length prefix) (String.length lower - String.length prefix)) with
           | w :: _ ->
             let clean = String.map (fun c -> if c = '(' || c = ')' || c = ':' || c = ';' then ' ' else c) w |> String.trim in
             (match words clean with
              | n :: _ when n <> "" -> Some { name = n; kind; line = line_no; exported = true }
              | _ -> None)
           | [] -> None)
        else go rest
    in
    go pick
  else if lang = "matlab" then
    (if starts lower "function" then
       let rest = String.sub t 8 (String.length t - 8) |> String.trim in
       let name =
         match find_sub rest "=" with
         | Some i ->
           let rhs = String.sub rest (i + 1) (String.length rest - i - 1) |> String.trim in
           (match find_sub rhs "(" with
            | Some j -> String.sub rhs 0 j |> String.trim
            | None -> (match words rhs with w :: _ -> w | [] -> ""))
         | None ->
           (match find_sub rest "(" with
            | Some j -> String.sub rest 0 j |> String.trim
            | None -> (match words rest with w :: _ -> w | [] -> ""))
       in
       if name = "" then None
       else Some { name; kind = "function"; line = line_no; exported = true }
     else if starts lower "classdef " then
       (match words (String.sub t 9 (String.length t - 9)) with
        | w :: _ -> Some { name = w; kind = "class"; line = line_no; exported = true }
        | [] -> None)
     else None)
  else if lang = "cobol" then
    (match find_sub lower "program-id." with
     | Some i ->
       let rest = String.sub t (i + 11) (String.length t - i - 11) in
       (match words rest with
        | w :: _ ->
          let n = String.map (fun c -> if c = '.' || c = ';' then ' ' else c) w |> String.trim in
          (match words n with x :: _ -> Some { name = x; kind = "program"; line = line_no; exported = true } | [] -> None)
        | [] -> None)
     | None -> None)
  else if lang = "abap" then
    let pick = [ ("class ", "class"); ("form ", "function"); ("function ", "function");
                 ("module ", "module"); ("interface ", "interface") ] in
    let rec go = function
      | [] -> None
      | (prefix, kind) :: rest ->
        if starts lower prefix then
          (match words (String.sub t (String.length prefix) (String.length t - String.length prefix)) with
           | w :: _ ->
             let n = String.map (fun c -> if c = '.' then ' ' else c) w |> String.trim in
             (match words n with x :: _ -> Some { name = x; kind; line = line_no; exported = true } | [] -> None)
           | [] -> None)
        else go rest
    in
    go pick
  else if lang = "typescript" || lang = "tsx" || lang = "javascript" then
    (let prims =
       [ ("createSignal(", "signal"); ("createStore(", "store");
         ("createProjection(", "projection"); ("createOptimisticStore(", "store");
         ("createMemo(", "memo"); ("action(", "action") ]
     in
     let kind =
       List.fold_left (fun acc (needle, k) ->
         match acc with
         | Some _ -> acc
         | None -> if contains t needle then Some k else None) None prims
     in
     match kind with
     | None -> first candidates
     | Some kind ->
    (match find_sub t "=" with
     | Some i ->
       let lhs = String.trim (String.sub t 0 i) in
       let lhs =
         if starts lhs "const " then String.sub lhs 6 (String.length lhs - 6) else lhs
       in
       let lhs = String.trim lhs in
       let name =
         if lhs <> "" && lhs.[0] = '[' then
           let inner = String.sub lhs 1 (String.length lhs - 1) in
           (match String.index_opt inner ',' with
            | Some j -> String.trim (String.sub inner 0 j)
            | None -> String.map (fun c -> if c = ']' then ' ' else c) inner |> String.trim)
         else match words lhs with w :: _ -> w | [] -> ""
       in
       if name = "" then first candidates
       else Some { name; kind; line = line_no; exported = true }
     | None -> first candidates))
  else if lang = "haskell" then
    match find_sub t " ::" with
    | Some i ->
      let lhs = String.sub t 0 i |> String.trim in
      (match words lhs with
       | [ n ] when n <> "" -> Some { name = n; kind = "function"; line = line_no; exported = true }
       | _ -> None)
    | None -> first candidates
  else if lang = "r" then
    let cut = match find_sub t "<- function" with
      | Some i -> Some i
      | None -> find_sub t "= function"
    in
    (match cut with
     | Some i ->
       (match words (String.sub t 0 i) with
        | n :: _ when n <> "" ->
          Some { name = n; kind = "function"; line = line_no; exported = true }
        | _ -> None)
     | None -> first candidates)
  else first candidates

let links content =
  let out = Hashtbl.create 16 in
  let n = String.length content in
  let add_from i stop =
    let j = ref i in
    while !j < n && content.[!j] <> stop && content.[!j] <> ' '
          && content.[!j] <> '\n' && content.[!j] <> ')' && content.[!j] <> '"' do
      incr j
    done;
    let link = String.sub content i (!j - i) in
    if link <> "" && link.[0] <> '#' && not (starts link "http")
       && not (starts link "mailto:") then Hashtbl.replace out link ();
    !j
  in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && content.[i] = ']' && content.[i + 1] = '(' then
      go (add_from (i + 2) ')')
    else if i + 5 < n && String.sub content i 6 = "href=\"" then
      go (add_from (i + 6) '"')
    else if i + 1 < n && content.[i] = '[' && content.[i + 1] = '[' then begin
      let j = ref (i + 2) in
      while !j + 1 < n && not (content.[!j] = ']' && content.[!j + 1] = ']') do incr j done;
      let link = String.sub content (i + 2) (!j - i - 2) in
      if link <> "" then Hashtbl.replace out link ();
      go (!j + 2)
    end else go (i + 1)
  in
  go 0;
  Hashtbl.fold (fun k () acc -> k :: acc) out []

let find_from s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go from

let line_at content i =
  let n = ref 1 in
  String.iteri (fun k c -> if k < i && c = '\n' then incr n) content;
  !n

let markdown_symbols content =
  let n = String.length content and out = ref [] and seen = Hashtbl.create 16 in
  let add name at =
    let name = String.trim name in
    let name =
      let n = String.length name in
      if n >= 2 && ((name.[0] = '(' && name.[n - 1] = ')')
                    || (name.[0] = '"' && name.[n - 1] = '"'))
      then String.sub name 1 (n - 2) else name
    in
    if name <> "" && String.length name < 60 && not (Hashtbl.mem seen name) then begin
      Hashtbl.replace seen name ();
      out := { name; kind = "label"; line = line_at content at; exported = true } :: !out
    end
  in
  let rec alts i =
    if i + 1 >= n then ()
    else if content.[i] = '!' && content.[i + 1] = '[' then begin
      let j = ref (i + 2) in
      while !j < n && content.[!j] <> ']' do incr j done;
      add (String.sub content (i + 2) (!j - i - 2)) i;
      alts (!j + 1)
    end else alts (i + 1)
  in
  alts 0;
  (match find_from content "```mermaid" 0 with
   | None -> ()
   | Some start ->
     (match find_from content "```" (start + 10) with
      | None -> ()
      | Some stop ->
        let i = ref start in
        while !i < stop do
          if content.[!i] = '[' then begin
            let j = ref (!i + 1) in
            while !j < stop && content.[!j] <> ']' do incr j done;
            if !j < stop then add (String.sub content (!i + 1) (!j - !i - 1)) !i;
            i := !j + 1
          end else incr i
        done));
  List.rev !out

let svg_symbols content =
  let n = String.length content and out = ref [] and seen = Hashtbl.create 16 in
  let add raw =
    let text = String.concat " " (words raw) |> String.trim in
    if text <> "" && String.length text < 80 && not (Hashtbl.mem seen text) then begin
      Hashtbl.replace seen text ();
      out := { name = text; kind = "label"; line = 0; exported = true } :: !out
    end
  in
  let rec go i =
    if i + 5 > n then ()
    else if String.sub content i 5 = "<text" then begin
      match find_from content ">" i with
      | None -> ()
      | Some start ->
        (match find_from content "</text>" (start + 1) with
         | None -> ()
         | Some stop ->
           add (String.sub content (start + 1) (stop - start - 1));
           go (stop + 7))
    end else go (i + 1)
  in
  go 0;
  List.rev !out

let call_names content =
  let s = blank_strings content in
  let n = String.length s and out = Hashtbl.create 64 in
  let is_id c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' in
  let rec go i =
    if i >= n then ()
    else if is_id s.[i] then begin
      let j = ref i in
      while !j < n && is_id s.[!j] do incr j done;
      let name = String.sub s i (!j - i) in
      if !j < n && s.[!j] = '(' then Hashtbl.replace out name ();
      go !j
    end else go (i + 1)
  in
  go 0;
  Hashtbl.fold (fun k () acc -> k :: acc) out []

type result = {
  imports : string list;
  calls : string list;
  symbols : symbol list;
}

let extract lang content =
  if lang = "markdown" then
    { imports = links content; calls = []; symbols = markdown_symbols content }
  else if lang = "pdf" then
    let _, _, topics = Pdf.extract content in
    { imports = []; calls = [];
      symbols = List.map (fun (name, _) ->
        { name; kind = "topic"; line = 0; exported = true }) topics }
  else if lang = "image" then
    { imports = []; calls = []; symbols = svg_symbols content }
  else begin
  let stripped = strip_comments (style_of_lang lang) content in
  let lines = String.split_on_char '\n' stripped in
  let imports = ref [] and symbols = ref [] in
  List.iteri (fun i line ->
    imports := List.rev_append (import_specs lang line) !imports;
    match symbol_of_line lang (i + 1) line with
    | Some s -> symbols := s :: !symbols
    | None -> ()
  ) lines;
  if lang = "ocaml" then imports := List.rev_append (module_refs stripped) !imports;
  { imports = List.rev !imports; calls = call_names stripped; symbols = List.rev !symbols }
  end
