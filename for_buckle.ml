Random.init 10;;

let split : Js.String.t -> Js.String.t -> Js.String.t list =
  fun s d ->
    Js.String.split s d
    |> Array.to_list

type request = {name:string; prefs:int list}

let list_sum = List.fold_left (+) 0

let pick_best eval = function
  | [] -> failwith "Must pick best from at least one option"
  | o :: os -> List.fold_left (fun a b -> if (eval a b) < 0 then a else b) o os

let rec gen_list_recur (gen:unit -> 'a) (count:int) (l:'a list) : 'a list =
  if count=0 then l
  else gen_list_recur gen (count-1) ((gen ()) :: l)

let gen_list gen count = gen_list_recur gen count []

module TreeCounter = struct
  type 'a t =
    | Leaf
    | Node of 'a * int * 'a t * 'a t

  let rec mem item = function
    | Leaf -> false
    | Node(x,_,l,r) ->
        if item=x then true
        else if item<x then mem item l
        else mem item r

  let rec inc item = function
    | Leaf -> (Node(item, 1, Leaf, Leaf), 1)
    | Node(x,c,l,r) ->
        if item=x then (Node(x, c+1, l, r), 0)
        else if item<x then
          let (new_l, size_diff) = inc item l in
          (Node(x, c, new_l, r), size_diff)
        else let (new_r, size_diff) = inc item r in
        (Node(x, c, l, new_r), size_diff)

  let rec min = function
    | Leaf -> failwith "Tree must have at least one entry"
    | Node(x,c,Leaf,_) -> (x,c)
    | Node(x,_,l,_) -> min l

  let rec dec_rec force_delete item = function
    | Leaf -> raise Not_found
    | Node(x,c,l,r) ->
        if item < x then
          let (new_l, size_diff) = dec_rec force_delete item l in
          (Node(x, c, new_l, r), size_diff) else
        if item > x then
          let (new_r, size_diff) = dec_rec force_delete item r in
          (Node(x, c, l, new_r), size_diff) else
        if (c > 1 && not force_delete) then (Node(x, c-1, l, r),0) else
        match (l,r) with
        | (Leaf,_) -> (r,~-1)
        | (_,Leaf) -> (l,~-1)
        | (_,_) ->
            let (new_val,new_count) = min r in
            let (r_without_min,_) = dec_rec true new_val r in
            (Node(new_val, new_count, l, r_without_min),~-1)

  let dec item tree =
    if not (mem item tree) then (tree,0)
    else dec_rec false item tree

  let rec size = function
    | Leaf -> 0
    | Node(_,_,l,r) -> 1 + size l + size r

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
  val optimize : (string -> unit) -> int -> int -> int -> input -> output
  val optimize_with_stats : (string -> unit) -> int -> int -> int -> input -> output
end

module Optimize (M : Optimizable) = struct
  (* include M *)
  type input = M.input
  type output = M.output

  let rec optimize_tree (breadth : int) (depth : int) (max_depth : int) (state : M.state) =
    if depth = max_depth then state else
    let do_try = fun () -> optimize_tree breadth (depth+1) max_depth (M.iter state) in
    let tries = gen_list do_try 5 in
    pick_best M.eval tries

  let rec optimize_loop (logf : string -> unit) skip_stats stop breadth max_depth times state =
    (if skip_stats then () else logf ((string_of_int times) ^ " times left."));
    if (stop logf state) && (times<=0) then state
    else let new_state = optimize_tree breadth 0 max_depth state in
    let chosen_state = pick_best M.eval [state; new_state] in
    let new_depth = if chosen_state == new_state then max_depth else (max_depth+1) in
    optimize_loop logf skip_stats stop breadth new_depth (times-1) chosen_state

  let _optimize (logf : string -> unit) skip_stats stop breadth depth times data =
    data |>
    M.state_of_data |>
    optimize_loop logf skip_stats stop breadth depth times |>
    (fun state -> if skip_stats then state else (logf (M.stats_of_state state); state)) |>
    M.output_of_state

  let whenever _ _ = true
  let optimize logf breadth depth times data = _optimize logf true whenever breadth depth times data
  let optimize_with_stats logf breadth depth times data = _optimize logf false whenever breadth depth times data
  let optimize_until logf stop breadth depth times data = _optimize logf false stop breadth depth times data
end

module MentorPrefs = struct
  type pref = int * int

  type input = request list

  type assignment = {name:string; mentor:int}
  type output = assignment list

  type request_with_assignment = {name:string; prefs:int list; mentor: int}
  type state = {
    choices:request_with_assignment list;
    deficit: int;
    rank_score: int;
    score: int;
    taken: int TreeCounter.t
  }

  type switch_data = {score_diff:int; old_mentor:int; new_mentor:int}

  let scored_pref : int list -> pref list = function
    | [first; second; third; fourth] ->
        [(first, 14); (second, 13); (third, 12); (fourth, 11)]
    | [first; second; third] ->
        [(first, 14); (second, 13); (third, 12)]
    | [first; second] ->
        [(first, 14); (second, 13)]
    | _ -> []

  let rec get_one_score pref m =
    match pref with
    | (pm, s) :: ps ->
        if pm=m then s
        else get_one_score ps m
    | [] -> failwith "Should only assign chosen mentors"

  let rec rank_score_rec score = function
    | [] -> score
    | {name;mentor;prefs} :: rest ->
        let updated_score = score + get_one_score (scored_pref prefs) mentor in
        rank_score_rec updated_score rest

  let rank_score = rank_score_rec 0

  let rec deficit_rec taken count = function
    | [] -> (TreeCounter.size taken) - count
    | {mentor} :: rest ->
        let (set_with_mentor,_) = TreeCounter.inc mentor taken in
        deficit_rec set_with_mentor (count+1) rest

  let deficit = deficit_rec TreeCounter.empty 0

  let adjusted_score assignments =
    let d = deficit assignments in
    if d<0 then d else rank_score assignments
  let eval a1 a2 = a2.score - a1.score

  let convert_pref ({name;prefs}:request) = match prefs with
    | [] -> failwith "Each student must have at least one preference"
    | p :: ps -> {name;prefs;mentor=p}

  let state_of_data d =
    let first_choices = List.map convert_pref d in
    let first_taken = (
      let assignments = List.map (fun {mentor} -> mentor) first_choices in
      let pass_counter item counter = (
        let (new_counter,_) = TreeCounter.inc item counter in new_counter
      ) in
      List.fold_right pass_counter assignments TreeCounter.empty
    ) in
    let first_score = adjusted_score first_choices in {
      score=first_score;
      rank_score=(rank_score first_choices);
      deficit=(deficit first_choices);
      choices=first_choices;
      taken=first_taken
    }

  let assignment_without_prefs ({name;mentor}:request_with_assignment) : assignment = {name;mentor}

  let output_of_state ({choices}:state) : output = List.map assignment_without_prefs choices

  let rec change_member changer i = function
    | [] -> failwith "change_member index should be less than list length"
    | x :: xs ->
        if i=0 then
          let (change_data, changed) = changer x in
          (change_data, changed :: xs)
        else let (change_data, changed_list) = change_member changer (i-1) xs in
        (change_data, x :: changed_list)

  let change_random_member changer l = change_member changer (List.length l |> Random.int) l

  let rec pick_random_rec cur seen = function
    | [] -> cur
    | x :: xs ->
        let choice = (if Random.int (seen+1) = 0 then Some x else cur)
        in pick_random_rec choice (seen+1) xs

  let pick_random = pick_random_rec None 0

  let rec get_rank_rec (r:request_with_assignment) count =
    let {name;mentor;prefs} = r in
    match prefs with
    | [] -> failwith "Must have at least one preference"
    | p :: ps -> if mentor=p then count else get_rank_rec {r with prefs=ps} (count+1)

  let get_rank a = get_rank_rec a 1

  let switch_mentor (r:request_with_assignment) : (switch_data * request_with_assignment) =
    let old_score = get_one_score (scored_pref r.prefs) r.mentor in
    let new_choice =
      match pick_random (List.filter ((!=) r.mentor) r.prefs) with
      | None -> failwith "Should have found a new mentor choice"
      | Some c -> c in
    let new_data = {r with mentor=new_choice} in
    let new_score = get_one_score (scored_pref new_data.prefs) new_data.mentor in
    ({score_diff=(new_score - old_score); old_mentor=r.mentor; new_mentor=new_choice}, new_data)

  let iter s =
    let (change_data, changed_prefs) = change_random_member switch_mentor s.choices in
    let (counter_after_dec, diff_after_dec) = TreeCounter.dec change_data.old_mentor s.taken in
    let (new_counter, diff_after_inc) = TreeCounter.inc change_data.new_mentor counter_after_dec in
    let deficit = s.deficit + diff_after_dec + diff_after_inc in
    let rank_score = s.rank_score + change_data.score_diff in
    {
      choices=changed_prefs;
      rank_score=rank_score;
      deficit=deficit;
      score=if deficit < 0 then deficit else rank_score;
      taken=new_counter
    }

  let mean l =
    let sum = float_of_int (list_sum l) in
    let length = float_of_int (List.length l) in
    sum /. length

  let mean_rank a = a |> List.map get_rank |> mean

  let stats_of_state (s:state) =
    if s.score < 0 then "Failed. Score: " ^ (string_of_int s.score) ^ "\n"
    else let a = s.choices in (
      "Score: " ^ (string_of_int s.score) ^ "\n" ^
      "1sts: " ^ (a |> List.map get_rank |> List.find_all ((=) 1) |> List.length |> string_of_int) ^ "\n" ^
      "2nds: " ^ (a |> List.map get_rank |> List.find_all ((=) 2) |> List.length |> string_of_int) ^ "\n" ^
      "3rds: " ^ (a |> List.map get_rank |> List.find_all ((=) 3) |> List.length |> string_of_int) ^ "\n" ^
      "4rds: " ^ (a |> List.map get_rank |> List.find_all ((=) 4) |> List.length |> string_of_int) ^ "\n" ^
      "Mean rank: " ^ (a |> mean_rank |> string_of_float)
    )

  let string_of_state ({choices}:state) =
    let make_line {name;mentor} = name ^ " " ^ (string_of_int mentor) ^ "\n" in
    let lines = List.map make_line choices in
    List.fold_right (^) lines ""

  let of_csv st =
    let convert_one_num maybe_num_str =
      try Some (int_of_string maybe_num_str) with
      | Failure int_of_string -> None in
    let list_of_options l =
      let process_one next accum = match next with
      | Some x -> x :: accum
      | None -> accum in
      List.fold_right process_one l [] in
    let process_line l = match (split "," l) with
    | [one_st;two_st;three_st;four_st;first_name;last_name] ->
        let prefs =
          [one_st;two_st;three_st;four_st] |>
          List.map convert_one_num |>
          list_of_options in
        {name=first_name^" "^last_name;prefs=prefs}
    | _ -> failwith "Each line must have exactly six entries."
    in
    split "\n" st |>
    List.map process_line
end

module PrefsOptimizer = Optimize(MentorPrefs)

let stop logf state =
  let open MentorPrefs in
  let mr = mean_rank state.choices in
  logf (stats_of_state state);
  logf (string_of_state state);
  state.score > 0 && mr < 0.5

let run csv logger stopper =
  let open PrefsOptimizer in
  MentorPrefs.of_csv csv |>
  optimize_until logger stopper 5 5 1

let run_from_csv csv = run csv print_endline stop
