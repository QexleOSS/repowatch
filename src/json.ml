(* Minimal JSON reader for LSP requests. Objects, arrays, strings, numbers. *)

type t =
  | Null
  | Bool of bool
  | Num of float
  | Str of string
  | Arr of t list
  | Obj of (string * t) list

exception Parse

let parse s =
  let n = String.length s in
  let i = ref 0 in
  let ws () =
    while !i < n && (match s.[!i] with ' ' | '\n' | '\r' | '\t' -> true | _ -> false) do incr i done
  in
  let lit text v =
    let m = String.length text in
    if !i + m <= n && String.sub s !i m = text then (i := !i + m; v) else raise Parse
  in
  let rec value () =
    ws ();
    if !i >= n then raise Parse;
    match s.[!i] with
    | '{' ->
      incr i; ws ();
      let items = ref [] in
      if !i < n && s.[!i] = '}' then incr i
      else begin
        let rec loop () =
          ws ();
          let k = str () in
          ws ();
          if !i >= n || s.[!i] <> ':' then raise Parse;
          incr i;
          let v = value () in
          items := (k, v) :: !items;
          ws ();
          if !i < n && s.[!i] = ',' then (incr i; loop ())
          else if !i < n && s.[!i] = '}' then incr i
          else raise Parse
        in
        loop ()
      end;
      Obj (List.rev !items)
    | '[' ->
      incr i; ws ();
      let items = ref [] in
      if !i < n && s.[!i] = ']' then incr i
      else begin
        let rec loop () =
          let v = value () in
          items := v :: !items;
          ws ();
          if !i < n && s.[!i] = ',' then (incr i; loop ())
          else if !i < n && s.[!i] = ']' then incr i
          else raise Parse
        in
        loop ()
      end;
      Arr (List.rev !items)
    | '"' -> Str (str ())
    | 't' -> lit "true" (Bool true)
    | 'f' -> lit "false" (Bool false)
    | 'n' -> lit "null" Null
    | _ -> num ()
  and num () =
    let start = !i in
    let is_num c =
      (c >= '0' && c <= '9') || c = '-' || c = '+' || c = '.' || c = 'e' || c = 'E'
    in
    while !i < n && is_num s.[!i] do incr i done;
    if !i = start then raise Parse;
    Num (float_of_string (String.sub s start (!i - start)))
  and str () =
    if !i >= n || s.[!i] <> '"' then raise Parse;
    incr i;
    let b = Buffer.create 16 in
    let rec go () =
      if !i >= n then raise Parse;
      match s.[!i] with
      | '"' -> incr i; Buffer.contents b
      | '\\' ->
        incr i;
        if !i >= n then raise Parse;
        (match s.[!i] with
         | 'n' -> Buffer.add_char b '\n'
         | 't' -> Buffer.add_char b '\t'
         | 'r' -> Buffer.add_char b '\r'
         | 'b' -> Buffer.add_char b '\b'
         | 'f' -> Buffer.add_char b '\012'
         | '"' -> Buffer.add_char b '"'
         | '\\' -> Buffer.add_char b '\\'
         | '/' -> Buffer.add_char b '/'
         | 'u' when !i + 4 < n ->
           (match int_of_string_opt ("0x" ^ String.sub s (!i + 1) 4) with
            | Some code when code < 128 -> Buffer.add_char b (Char.chr code)
            | _ -> Buffer.add_char b '?');
           i := !i + 4
         | _ -> raise Parse);
        incr i;
        go ()
      | c -> Buffer.add_char b c; incr i; go ()
    in
    go ()
  in
  let v = value () in
  ws ();
  v

let member key = function
  | Obj xs -> List.assoc_opt key xs
  | _ -> None

let string = function Str s -> Some s | _ -> None
let number = function Num n -> Some n | _ -> None
