(* Graph math: PageRank over import/call edges, and strongly connected
   components for cycle reporting. *)

type edge = { src : int; dst : int; kind : int (* 0 import, 1 call *) }

let weights = [| 0.4; 0.6 |]

let pagerank n edges =
  let out = Array.make n [] and deg = Array.make n 0.0 in
  Array.iter (fun e ->
    out.(e.src) <- e :: out.(e.src);
    deg.(e.src) <- deg.(e.src) +. weights.(e.kind)
  ) edges;
  let scores = Array.make n (1.0 /. float_of_int (max 1 n)) in
  let next = Array.make n 0.0 in
  for _ = 1 to 40 do
    let base = (1.0 -. 0.85) /. float_of_int (max 1 n) in
    Array.fill next 0 n base;
    let dangling = ref 0.0 in
    for i = 0 to n - 1 do
      if deg.(i) = 0.0 then dangling := !dangling +. scores.(i)
      else List.iter (fun e ->
        next.(e.dst) <- next.(e.dst) +. 0.85 *. scores.(i) *. weights.(e.kind) /. deg.(i)
      ) out.(i)
    done;
    let share = 0.85 *. !dangling /. float_of_int (max 1 n) in
    for i = 0 to n - 1 do next.(i) <- next.(i) +. share done;
    Array.blit next 0 scores 0 n
  done;
  scores

let in_degree n edges =
  let d = Array.make n 0 in
  Array.iter (fun e -> if e.src <> e.dst then d.(e.dst) <- d.(e.dst) + 1) edges;
  d

(* Tarjan, recursive; the stack is small next to OCaml's default. *)
let cycles n edges =
  let adj = Array.make n [] in
  Array.iter (fun e -> if e.src <> e.dst then adj.(e.src) <- e.dst :: adj.(e.src)) edges;
  let index = Array.make n (-1) and low = Array.make n 0 and on = Array.make n false in
  let stack = ref [] and counter = ref 0 and out = ref [] in
  let rec strong v =
    index.(v) <- !counter; low.(v) <- !counter; incr counter;
    stack := v :: !stack; on.(v) <- true;
    List.iter (fun w ->
      if index.(w) < 0 then begin
        strong w;
        low.(v) <- min low.(v) low.(w)
      end else if on.(w) then low.(v) <- min low.(v) index.(w)
    ) adj.(v);
    if low.(v) = index.(v) then begin
      let rec pop acc =
        match !stack with
        | [] -> acc
        | w :: rest ->
          stack := rest; on.(w) <- false;
          if w = v then w :: acc else pop (w :: acc)
      in
      let comp = pop [] in
      if List.length comp > 1 then out := comp :: !out
    end
  in
  for i = 0 to n - 1 do if index.(i) < 0 then strong i done;
  List.sort (fun a b -> compare (List.length b) (List.length a)) !out
