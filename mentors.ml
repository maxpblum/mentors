Random.init 10;;

let prefs : (string * (int list)) list = [
  ("Jim", [17; 26; 26; 25]);
  ("Jim", [22; 13; 26; 33]);
  ("Jim", [12; 4; 2; 23]);
  ("Jim", [38; 18; 12; 10]);
  ("Jim", [3; 15; 36; 1]);
  ("Jim", [33; 1; 39; 29]);
  ("Jim", [5; 35; 32; 19]);
  ("Jim", [38; 7; 16; 21]);
  ("Jim", [18; 39; 20; 26]);
  ("Jim", [39; 17; 32; 13]);
  ("Jim", [25; 8; 18; 33]);
  ("Jim", [15; 25; 3; 15]);
  ("Jim", [28; 25; 8; 20]);
  ("Jim", [15; 21; 1; 13]);
  ("Jim", [38; 32; 31; 4]);
  ("Jim", [31; 19; 31; 2]);
  ("Jim", [18; 38; 34; 35]);
  ("Jim", [5; 29; 2; 31]);
  ("Jim", [24; 22; 30; 36]);
  ("Jim", [6; 14; 17; 14]);
]

let list_sum = List.fold_left (+) 0

let pick_best eval = function
  | [] -> failwith "Must pick best from at least one option"
  | o :: os -> List.fold_left (fun a b -> if (eval a b) < 0 then a else b) o os

let rec gen_list_recur (gen:unit -> 'a) (count:int) (l:'a list) : 'a list =
  if count=0 then l
  else gen_list_recur gen (count-1) ((gen ()) :: l)

let gen_list gen count = gen_list_recur gen count []

module type Set = sig
  type 'a t
  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  val size : 'a t -> int
end

module TreeSet = struct
  type 'a t =
    | Leaf
    | Node of 'a * 'a t * 'a t

  let rec add item = function
    | Leaf -> Node(item,Leaf,Leaf)
    | Node(x,l,r) ->
        if item=x then Node(x,l,r)
        else if item < x then Node(x, (add item l), r)
        else Node(x, l, (add item r))

  let rec size = function
    | Leaf -> 0
    | Node(_,l,r) -> 1 + size l + size r

  let empty = Leaf
end

module type Optimizable = sig
  type input
  type output
  type state
  val iter : state -> state
  val eval : state -> state -> int
  val state_of_data : input -> state
  val output_of_state : state -> output
  val stats_of_state : state -> string
end

module type Optimize = sig
  type input
  type output
  val optimize : int -> int -> int -> input -> output
  val optimize_with_stats : int -> int -> int -> input -> output
end

module Optimize (M : Optimizable) = struct
  type input = M.input
  type output = M.output

  let rec optimize_tree breadth depth max_depth state =
    if depth = max_depth then state else
    let do_try = fun () -> optimize_tree breadth (depth+1) max_depth (M.iter state) in
    let tries = gen_list do_try 5 in
    pick_best M.eval tries

  let rec optimize_loop breadth max_depth times state =
    if times=0 then state
    else let new_state = optimize_tree breadth 0 max_depth state in
    optimize_loop breadth max_depth (times-1) new_state

  let _optimize skip_stats breadth depth times data =
    data |>
    M.state_of_data |>
    optimize_loop breadth depth times |>
    (fun state -> if skip_stats then state else (print_endline (M.stats_of_state state); state)) |>
    M.output_of_state

  let optimize = _optimize true
  let optimize_with_stats = _optimize false
end

module MentorPrefs = struct
  type pref = int * int

  type input = (string * (int list)) list
  type output = (string * int) list
  type state = (string * int * (int list)) list

  let scored_pref : int list -> pref list = function
    | [first; second; third; fourth] ->
        [(first, 14); (second, 13); (third, 12); (fourth, 11)]
    | [first; second; third] ->
        [(first, 14); (second, 13); (third, 12)]
    | _ -> []

  let convert_pref (n,pref) = match pref with
    | [] -> failwith "Each student must have at least one preference"
    | p :: ps -> (n, p, pref)

  let state_of_data = List.map convert_pref
  let output_of_state = List.map (fun (name,mentor,prefs) -> (name,mentor))

  let rec get_one_score pref m =
    match pref with
    | (pm, s) :: ps ->
        if pm=m then s
        else get_one_score ps m
    | [] -> failwith "Should only assign chosen mentors"

  let rec score_set_rec assignments taken score count = match assignments with
  | [] -> let diff = (TreeSet.size taken) - count in
    if diff != 0 then diff else score
  | (n,m,pref) :: rest ->
    score_set_rec rest (TreeSet.add m taken) (score + get_one_score (scored_pref pref) m) (count+1)

  let score_set assignments = score_set_rec assignments TreeSet.empty 0 0
  let eval a1 a2 = (score_set a2) - (score_set a1)

  let rec change_member changer i = function
    | [] -> failwith "change_member index should be less than list length"
    | x :: xs ->
        if i=0 then (changer x) :: xs
        else x :: change_member changer (i-1) xs

  let change_random_member changer l = change_member changer (List.length l |> Random.int) l

  let rec pick_random_rec cur seen = function
    | [] -> cur
    | x :: xs ->
        let choice = (if Random.int (seen+1) = 0 then Some x else cur)
        in pick_random_rec choice (seen+1) xs

  let pick_random = pick_random_rec None 0

  let switch_mentor (n, m, pref) =
    let new_choice =
      match pick_random (List.filter ((!=) m) pref) with
      | None -> failwith "Should have found a new mentor choice"
      | Some c -> c
      in (n, new_choice, pref)

  let iter = change_random_member switch_mentor

  let rec get_rank_rec (n,m,pref) count = match pref with
    | [] -> failwith "Must have at least one preference"
    | p :: ps -> if m=p then count else get_rank_rec (n,m,ps) (count+1)

  let get_rank a = get_rank_rec a 1

  let mean l =
    let sum = float_of_int (list_sum l) in
    let length = float_of_int (List.length l) in
    sum /. length

  let mean_rank a = a |> List.map get_rank |> mean

  let stats_of_state a =
    let score = score_set a in
    if score < 0 then "Failed.\n"
    else (
      "Score: " ^ (string_of_int score) ^ "\n" ^
      "1sts: " ^ (a |> List.map get_rank |> List.find_all ((=) 1) |> List.length |> string_of_int) ^ "\n" ^
      "2nds: " ^ (a |> List.map get_rank |> List.find_all ((=) 2) |> List.length |> string_of_int) ^ "\n" ^
      "3rds: " ^ (a |> List.map get_rank |> List.find_all ((=) 3) |> List.length |> string_of_int) ^ "\n" ^
      "4rds: " ^ (a |> List.map get_rank |> List.find_all ((=) 4) |> List.length |> string_of_int) ^ "\n" ^
      "Mean rank: " ^ (a |> mean_rank |> string_of_float)
    )
end

module PrefsOptimizer = Optimize(MentorPrefs)

let generate_requests student_count mentor_count =
  let gen_student = fun () -> ("Jim", List.map (fun () -> 1 + Random.int mentor_count) [(); (); (); ()]) in
  gen_list gen_student student_count
