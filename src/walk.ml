(* Directory walk. Skips noise and honours .gitignore, one file per directory. *)

let skipped =
  [ ".git"; ".hg"; ".svn"; "node_modules"; "dist"; "build"; "target";
    "_build"; ".repowatch"; ".next"; ".venv"; "venv"; "__pycache__";
    "vendor"; ".cache"; "coverage" ]

let max_bytes = 2 * 1024 * 1024

type pat = { body : string; neg : bool; dir_only : bool; anchored : bool; base : string }

let rec glob p i s j =
  if i >= String.length p then j >= String.length s
  else match p.[i] with
    | '*' ->
      let dbl = i + 1 < String.length p && p.[i + 1] = '*' in
      let next = if dbl && i + 2 < String.length p && p.[i + 2] = '/' then i + 3 else i + 1 in
      glob p next s j
      || (j < String.length s && (dbl || s.[j] <> '/') && glob p i s (j + 1))
    | '?' -> j < String.length s && s.[j] <> '/' && glob p (i + 1) s (j + 1)
    | c -> j < String.length s && s.[j] = c && glob p (i + 1) s (j + 1)

let parse base line =
  let line = String.trim line in
  if line = "" || line.[0] = '#' then []
  else
    let neg = line.[0] = '!' in
    let body = if neg then String.sub line 1 (String.length line - 1) else line in
    let dir_only = body <> "" && body.[String.length body - 1] = '/' in
    let body = if dir_only then String.sub body 0 (String.length body - 1) else body in
    if body = "" then []
    else
      let anchored = String.contains body '/' || body.[0] = '/' in
      let body = if body.[0] = '/' then String.sub body 1 (String.length body - 1) else body in
      [ { body; neg; dir_only; anchored; base } ]

let read_file p base pats =
  match In_channel.with_open_bin p In_channel.input_all with
  | exception Sys_error _ -> pats
  | content ->
    pats @ (String.split_on_char '\n' content |> List.concat_map (parse base))

let read_ignore root dir pats =
  read_file (Filename.concat root (Filename.concat dir ".gitignore")) dir pats

(* git: a pattern with no slash matches the name at any depth below its file. *)
let matches p rel is_dir =
  let base = if p.base = "" then "" else p.base ^ "/" in
  let n = String.length base in
  String.length rel >= n && String.sub rel 0 n = base
  && (not p.dir_only || is_dir)
  && (let sub = String.sub rel n (String.length rel - n) in
      glob p.body 0 (if p.anchored then sub else Filename.basename sub) 0)

let ignored pats rel is_dir =
  List.fold_left (fun acc p -> if matches p rel is_dir then not p.neg else acc) false pats

let rec walk_into root dir pats acc =
  let abs = if dir = "" then root else Filename.concat root dir in
  let pats = read_ignore root dir pats in
  match Sys.readdir abs with
  | exception Sys_error _ -> acc
  | entries ->
    Array.sort compare entries;
    Array.fold_left (fun acc name ->
      let rel = if dir = "" then name else Filename.concat dir name in
      let full = Filename.concat root rel in
      match Unix.lstat full with
      | exception Unix.Unix_error _ -> acc
      | st ->
        if st.Unix.st_kind = Unix.S_DIR then
          if List.mem name skipped || name.[0] = '.' || ignored pats rel true then acc
          else walk_into root rel pats acc
        else if st.Unix.st_kind = Unix.S_REG && st.Unix.st_size <= max_bytes
             && not (ignored pats rel false) then
          (rel, st) :: acc
        else acc
    ) acc entries

let walk root =
  let pats = read_file (Filename.concat root ".git/info/exclude") "" [] in
  List.rev (walk_into root "" pats [])
