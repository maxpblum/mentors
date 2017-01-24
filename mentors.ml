Random.init 10;;

type request = {name:string; prefs:int list}

let sample_prefs : request list = [
  {name="Jim"; prefs=[17; 26; 26; 25]};
  {name="Jim"; prefs=[22; 13; 26; 33]};
  {name="Jim"; prefs=[12; 4; 2; 23]};
  {name="Jim"; prefs=[38; 18; 12; 10]};
  {name="Jim"; prefs=[3; 15; 36; 1]};
  {name="Jim"; prefs=[33; 1; 39; 29]};
  {name="Jim"; prefs=[5; 35; 32; 19]};
  {name="Jim"; prefs=[38; 7; 16; 21]};
  {name="Jim"; prefs=[18; 39; 20; 26]};
  {name="Jim"; prefs=[39; 17; 32; 13]};
  {name="Jim"; prefs=[25; 8; 18; 33]};
  {name="Jim"; prefs=[15; 25; 3; 15]};
  {name="Jim"; prefs=[28; 25; 8; 20]};
  {name="Jim"; prefs=[15; 21; 1; 13]};
  {name="Jim"; prefs=[38; 32; 31; 4]};
  {name="Jim"; prefs=[31; 19; 31; 2]};
  {name="Jim"; prefs=[18; 38; 34; 35]};
  {name="Jim"; prefs=[5; 29; 2; 31]};
  {name="Jim"; prefs=[24; 22; 30; 36]};
  {name="Jim"; prefs=[6; 14; 17; 14]};
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
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val min : 'a t -> 'a
  val remove : 'a -> 'a t -> 'a t
  val size : 'a t -> int
end

module TreeSet = struct
  type 'a t =
    | Leaf
    | Node of 'a * 'a t * 'a t

  let rec search item if_leaf if_node = function
    | Leaf -> if_leaf
    | Node(x,l,r) ->
        if item=x then if_node l r
        else if item<x then search item if_leaf if_node l
        else search item if_leaf if_node r

  let mem item = search item false (fun _ _ -> true)
  let add item = search item (Node(item, Leaf, Leaf)) (fun l r -> Node(item,l,r))

  let rec min = function
    | Leaf -> failwith "Tree must have at least one entry"
    | Node(x,Leaf,_) -> x
    | Node(x,l,_) -> min l

  let rec remove item = function
    | Leaf -> raise Not_found
    | Node(x,l,_)    when item < x -> remove item l
    | Node(x,_,r)    when item > x -> remove item r
    | Node(x,Leaf,r) when item = x -> r
    | Node(x,l,Leaf) when item = x -> l
    | Node(x,l,r)                  ->
        let new_val = min r in
        let r_without_min = remove new_val r in
        Node(new_val, l, r_without_min)

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
  include M
  (* type input = M.input *)
  (* type output = M.output *)

  let rec optimize_tree breadth depth max_depth state =
    if depth = max_depth then state else
    let do_try = fun () -> optimize_tree breadth (depth+1) max_depth (M.iter state) in
    let tries = gen_list do_try 5 in
    pick_best M.eval tries

  let rec optimize_loop skip_stats stop breadth max_depth times state =
    (if skip_stats then () else print_endline ((string_of_int times) ^ " times left."));
    if (stop state) && (times=0) then state
    else let new_state = optimize_tree breadth 0 max_depth state in
    let chosen_state = pick_best M.eval [state; new_state] in
    let new_depth = if chosen_state == new_state then max_depth else (max_depth+1) in
    optimize_loop skip_stats stop breadth new_depth (times-1) chosen_state

  let _optimize skip_stats stop breadth depth times data =
    data |>
    M.state_of_data |>
    optimize_loop skip_stats stop breadth depth times |>
    (fun state -> if skip_stats then state else (print_endline (M.stats_of_state state); state)) |>
    M.output_of_state

  let whenever _ = true
  let optimize = _optimize true whenever
  let optimize_with_stats = _optimize false whenever
  let optimize_until stop = _optimize false stop
end

module MentorPrefs = struct
  type pref = int * int

  type input = request list

  type assignment = {name:string; mentor:int}
  type output = assignment list

  type request_with_assignment = {name:string; prefs:int list; mentor: int}
  type state = request_with_assignment list

  let scored_pref : int list -> pref list = function
    | [first; second; third; fourth] ->
        [(first, 14); (second, 13); (third, 12); (fourth, 11)]
    | [first; second; third] ->
        [(first, 14); (second, 13); (third, 12)]
    | _ -> []

  let convert_pref ({name;prefs}:request) = match prefs with
    | [] -> failwith "Each student must have at least one preference"
    | p :: ps -> {name;prefs;mentor=p}

  let state_of_data = List.map convert_pref
  let output_of_state : state -> output = List.map (fun {name;mentor;prefs} -> {name;mentor})

  let rec get_one_score pref m =
    match pref with
    | (pm, s) :: ps ->
        if pm=m then s
        else get_one_score ps m
    | [] -> failwith "Should only assign chosen mentors"

  let rec score_set_rec (assignments:state) taken score count = match assignments with
  | [] ->
      let diff = (TreeSet.size taken) - count in
      if diff != 0 then diff else score
  | {name;mentor;prefs} :: rest ->
      let set_with_mentor = TreeSet.add mentor taken in
      let updated_score = score + get_one_score (scored_pref prefs) mentor in
      score_set_rec rest set_with_mentor updated_score (count+1)

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

  let switch_mentor (r:request_with_assignment) =
    let new_choice =
      match pick_random (List.filter ((!=) r.mentor) r.prefs) with
      | None -> failwith "Should have found a new mentor choice"
      | Some c -> c
    in {r with mentor=new_choice}

  let iter = change_random_member switch_mentor

  let rec get_rank_rec (r:request_with_assignment) count =
    let {name;mentor;prefs} = r in
    match prefs with
    | [] -> failwith "Must have at least one preference"
    | p :: ps -> if mentor=p then count else get_rank_rec {r with prefs=ps} (count+1)

  let get_rank a = get_rank_rec a 1

  let mean l =
    let sum = float_of_int (list_sum l) in
    let length = float_of_int (List.length l) in
    sum /. length

  let mean_rank a = a |> List.map get_rank |> mean

  let stats_of_state a =
    let score = score_set a in
    if score < 0 then "Failed. Score: " ^ (string_of_int score) ^ "\n"
    else (
      "Score: " ^ (string_of_int score) ^ "\n" ^
      "1sts: " ^ (a |> List.map get_rank |> List.find_all ((=) 1) |> List.length |> string_of_int) ^ "\n" ^
      "2nds: " ^ (a |> List.map get_rank |> List.find_all ((=) 2) |> List.length |> string_of_int) ^ "\n" ^
      "3rds: " ^ (a |> List.map get_rank |> List.find_all ((=) 3) |> List.length |> string_of_int) ^ "\n" ^
      "4rds: " ^ (a |> List.map get_rank |> List.find_all ((=) 4) |> List.length |> string_of_int) ^ "\n" ^
      "Mean rank: " ^ (a |> mean_rank |> string_of_float)
    )

  let string_of_state (a:state) =
    let make_line {name;mentor} = name ^ " " ^ (string_of_int mentor) ^ "\n" in
    let lines = List.map make_line a in
    List.fold_right (^) lines ""
end

module PrefsOptimizer = Optimize(MentorPrefs)

let generate_requests student_count mentor_count =
  let gen_student = fun () -> ("Jim", List.map (fun () -> 1 + Random.int mentor_count) [(); (); (); ()]) in
  gen_list gen_student student_count

let trench_b : MentorPrefs.input = [
  {name="Redacted Redacted"; prefs=[170;216;115;201]};
  {name="Redacted Redacted"; prefs=[52;130;192;71]};
  {name="Redacted Redacted"; prefs=[163;206;85;118]};
  {name="Redacted Redacted"; prefs=[163;209;84;45]};
  {name="Redacted Redacted"; prefs=[169;194;163;76]};
  {name="Redacted Redacted"; prefs=[90;107;187;154]};
  {name="Redacted Redacted"; prefs=[163;166;162;6]};
  {name="Redacted Redacted"; prefs=[163;176;211;130]};
  {name="Redacted Redacted"; prefs=[79;56;116;118]};
  {name="Redacted Redacted"; prefs=[162;147;139;3]};
  {name="Redacted Redacted"; prefs=[201;162;71;9]};
  {name="Redacted Redacted"; prefs=[163;179;80;194]};
  {name="Redacted Redacted"; prefs=[16;95;88;56]};
  {name="Redacted Redacted"; prefs=[204;101;28;181]};
  {name="Redacted Redacted"; prefs=[185;124;186;197]};
  {name="Redacted Redacted"; prefs=[215;216;169;107]};
  {name="Redacted Redacted"; prefs=[215;216;169;42]};
  {name="Redacted Redacted"; prefs=[6;160;45;174]};
  {name="Redacted Redacted"; prefs=[163;107;28;179]};
  {name="Redacted Redacted"; prefs=[165;113;119;35]};
  {name="Redacted Redacted"; prefs=[215;190;173;124]};
  {name="Redacted Redacted"; prefs=[71;194;53;42]};
  {name="Redacted Redacted"; prefs=[201;163;167;151]};
  {name="Redacted Redacted"; prefs=[160;145;116;217]};
  {name="Redacted Redacted"; prefs=[49;214;69;88]};
  {name="Redacted Redacted"; prefs=[215;60;42;147]};
  {name="Redacted Redacted"; prefs=[182;197;97;136]};
  {name="Redacted Redacted"; prefs=[167;194;139;71]};
  {name="Redacted Redacted"; prefs=[185;194;160;163]};
  {name="Redacted Redacted"; prefs=[19;93;171;197]};
  {name="Redacted Redacted"; prefs=[163;71;215;107]};
  {name="Redacted Redacted"; prefs=[53;65;147;110]};
  {name="Redacted Redacted"; prefs=[215;188;184;107]};
  {name="Redacted Redacted"; prefs=[107;139;163;59]};
  {name="Redacted Redacted"; prefs=[169;163;42;106]};
  {name="Redacted Redacted"; prefs=[139;107;172;197]};
  {name="Redacted Redacted"; prefs=[169;194;126;151]};
  {name="Redacted Redacted"; prefs=[57;45;63;90]};
  {name="Redacted Redacted"; prefs=[209;161;162;202]};
  {name="Redacted Redacted"; prefs=[169;139;151;192]};
  {name="Redacted Redacted"; prefs=[139;71;45;163]};
  {name="Redacted Redacted"; prefs=[126;163;201;169]};
  {name="Redacted Redacted"; prefs=[106;204;99;169]};
  {name="Redacted Redacted"; prefs=[68;141;146;80]};
  {name="Redacted Redacted"; prefs=[95;4;107;110]};
  {name="Redacted Redacted"; prefs=[160;191;53;19]};
  {name="Redacted Redacted"; prefs=[170;216;115;201]};
  {name="Redacted Redacted"; prefs=[104;85;19;74]};
  {name="Redacted Redacted"; prefs=[90;148;161;216]};
  {name="Redacted Redacted"; prefs=[216;42;167;115]};
  {name="Redacted Redacted"; prefs=[204;127;194;88]};
  {name="Redacted Redacted"; prefs=[107;160;56;199]};
  {name="Redacted Redacted"; prefs=[49;107;182;79]};
  {name="Redacted Redacted"; prefs=[193;88;207;217]};
  {name="Redacted Redacted"; prefs=[188;216;90;18]};
  {name="Redacted Redacted"; prefs=[139;107;73;167]};
  {name="Redacted Redacted"; prefs=[185;124;19;197]};
  {name="Redacted Redacted"; prefs=[119;126;35;194]};
  {name="Redacted Redacted"; prefs=[3;145;217;71]};
  {name="Redacted Redacted"; prefs=[104;197;182;93]};
  {name="Redacted Redacted"; prefs=[127;109;44;]};
  {name="Redacted Redacted"; prefs=[163;204;85;160]};
  {name="Redacted Redacted"; prefs=[76;130;42;167]};
  {name="Redacted Redacted"; prefs=[145;209;162;217]};
  {name="Redacted Redacted"; prefs=[216;169;115;194]};
  {name="Redacted Redacted"; prefs=[169;163;76;]};
  {name="Redacted Redacted"; prefs=[184;197;76;167]};
  {name="Redacted Redacted"; prefs=[79;85;35;167]};
  {name="Redacted Redacted"; prefs=[71;110;119;204]};
  {name="Redacted Redacted"; prefs=[18;9;139;138]};
  {name="Redacted Redacted"; prefs=[71;129;204;210]};
  {name="Redacted Redacted"; prefs=[132;87;103;66]};
  {name="Redacted Redacted"; prefs=[103;73;125;98]};
]

let stop state =
  let open MentorPrefs in
  let mr = mean_rank state in
  let sc = score_set state in
  print_endline (stats_of_state state);
  print_string (string_of_state state);
  sc > 0 && mr < 3.0
