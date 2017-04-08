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
    if (stop state) && (times<=0) then state
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
    let process_line l = match (String.split_on_char ',' l) with
    | [one_st;two_st;three_st;four_st;first_name;last_name] ->
        let prefs =
          [one_st;two_st;three_st;four_st] |>
          List.map convert_one_num |>
          list_of_options in
        {name=first_name^" "^last_name;prefs=prefs}
    | _ -> failwith "Each line must have exactly six entries."
    in
    st |>
    String.split_on_char '\n' |>
    List.map process_line
end

module PrefsOptimizer = Optimize(MentorPrefs)

let generate_requests student_count mentor_count =
  let gen_student = fun () -> ("Jim", List.map (fun () -> 1 + Random.int mentor_count) [(); (); (); ()]) in
  gen_list gen_student student_count

let trench_a : MentorPrefs.input = [
  {prefs=[20;91;93;94]; name="Redacted Redacted"};
  {prefs=[46;72;36;21]; name="Redacted Redacted"};
  {prefs=[92;86;61;4]; name="Redacted Redacted"};
  {prefs=[36;153;66;48]; name="Redacted Redacted"};
  {prefs=[33;129;61;107]; name="Redacted Redacted"};
  {prefs=[25;122;37;55]; name="Redacted Redacted"};
  {prefs=[66;25;43;122]; name="Redacted Redacted"};
  {prefs=[24;122;58;72]; name="Redacted Redacted"};
  {prefs=[47;92;105;71]; name="Redacted Redacted"};
  {prefs=[39;14;27;145]; name="Redacted Redacted"};
  {prefs=[72;120;55;92]; name="Redacted Redacted"};
  {prefs=[27;77;92;71]; name="Redacted Redacted"};
  {prefs=[140;146;10;26]; name="Redacted Redacted"};
  {prefs=[140;153;151;48]; name="Redacted Redacted"};
  {prefs=[146;25;41;107]; name="Redacted Redacted"};
  {prefs=[27;14;61;42]; name="Redacted Redacted"};
  {prefs=[39;21;145;137]; name="Redacted Redacted"};
  {prefs=[58;25;88;152]; name="Redacted Redacted"};
  {prefs=[36;137;85;96]; name="Redacted Redacted"};
  {prefs=[25;108;82;27]; name="Redacted Redacted"};
  {prefs=[111;149;135]; name="Redacted Redacted"};
  {prefs=[121;23;8;54]; name="Redacted Redacted"};
  {prefs=[26;92;47;19]; name="Redacted Redacted"};
  {prefs=[14;58;98;126]; name="Redacted Redacted"};
  {prefs=[68;77;107;37]; name="Redacted Redacted"};
  {prefs=[13;122;127;109]; name="Redacted Redacted"};
  {prefs=[47;154;71;25]; name="Redacted Redacted"};
  {prefs=[120;72;68;26]; name="Redacted Redacted"};
  {prefs=[144;71;27;92]; name="Redacted Redacted"};
  {prefs=[128;15;81;107]; name="Redacted Redacted"};
  {prefs=[78;37;128;86]; name="Redacted Redacted"};
  {prefs=[24;149;40;26]; name="Redacted Redacted"};
  {prefs=[61;22;147;122]; name="Redacted Redacted"};
  {prefs=[12;22;78;47]; name="Redacted Redacted"};
  {prefs=[141;102;118;14]; name="Redacted Redacted"};
  {prefs=[142;38;104;50]; name="Redacted Redacted"};
  {prefs=[51;35;]; name="Redacted Redacted"};
  {prefs=[81;79;114;25]; name="Redacted Redacted"};
  {prefs=[7;3;17;43]; name="Redacted Redacted"};
  {prefs=[68;134;111;153]; name="Redacted Redacted"};
  {prefs=[26;72;120;134]; name="Redacted Redacted"};
  {prefs=[81;119;68;79]; name="Redacted Redacted"};
  {prefs=[149;68;49;146]; name="Redacted Redacted"};
  {prefs=[145;27;150;39]; name="Redacted Redacted"};
  {prefs=[84;140;133;25]; name="Redacted Redacted"};
  {prefs=[77;68;37;60]; name="Redacted Redacted"};
  {prefs=[2;37;33;110]; name="Redacted Redacted"};
  {prefs=[36;153;48;140]; name="Redacted Redacted"};
  {prefs=[94;79;43;88]; name="Redacted Redacted"};
  {prefs=[26;120;68;72]; name="Redacted Redacted"};
  {prefs=[64;29;57;43]; name="Redacted Redacted"};
  {prefs=[26;77;66;121]; name="Redacted Redacted"};
  {prefs=[105;27;92;14]; name="Redacted Redacted"};
  {prefs=[68;27;25;5]; name="Redacted Redacted"};
  {prefs=[135;94;43;36]; name="Redacted Redacted"};
  {prefs=[121;62;14;126]; name="Redacted Redacted"};
  {prefs=[78;100;152;146]; name="Redacted Redacted"};
  {prefs=[122;27;55;128]; name="Redacted Redacted"};
  {prefs=[22;61;47;126]; name="Redacted Redacted"};
  {prefs=[146;145;69;140]; name="Redacted Redacted"};
  {prefs=[128;41;61;73]; name="Redacted Redacted"};
  {prefs=[68;152;71;137]; name="Redacted Redacted"};
]

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

let trench_c =
"17,9,29,70,Redacted,Redacted
373,337,8,318,Redacted,Redacted
340,211,443,272,Redacted,Redacted
248,436,65,181,Redacted,Redacted
436,243,431,340,Redacted,Redacted
313,309,233,304,Redacted,Redacted
345,304,444,458,Redacted,Redacted
237,457,254,357,Redacted,Redacted
434,187,432,350,Redacted,Redacted
29,436,147,440,Redacted,Redacted
375,408,449,,Redacted,Redacted
93,237,355,442,Redacted,Redacted
364,346,309,289,Redacted,Redacted
416,211,239,123,Redacted,Redacted
420,432,373,350,Redacted,Redacted
307,217,250,138,Redacted,Redacted
309,373,432,308,Redacted,Redacted
436,273,118,267,Redacted,Redacted
340,440,271,451,Redacted,Redacted
217,428,168,328,Redacted,Redacted
8,157,237,313,Redacted,Redacted
181,328,213,436,Redacted,Redacted
10,17,,,Redacted,Redacted
420,345,419,350,Redacted,Redacted
171,281,241,457,Redacted,Redacted
221,4,328,117,Redacted,Redacted
241,93,254,355,Redacted,Redacted
309,373,432,308,Redacted,Redacted
10,440,29,451,Redacted,Redacted
238,9,440,248,Redacted,Redacted
248,436,4,427,Redacted,Redacted
252,298,423,409,Redacted,Redacted
416,233,448,247,Redacted,Redacted
444,252,313,231,Redacted,Redacted
242,138,181,17,Redacted,Redacted
448,233,373,451,Redacted,Redacted
432,434,233,318,Redacted,Redacted
211,118,273,217,Redacted,Redacted
67,216,403,450,Redacted,Redacted
32,48,217,318,Redacted,Redacted
340,263,273,271,Redacted,Redacted
316,250,247,4,Redacted,Redacted
240,248,402,350,Redacted,Redacted
421,380,138,9,Redacted,Redacted
402,428,174,444,Redacted,Redacted
357,337,397,428,Redacted,Redacted
248,340,439,233,Redacted,Redacted
138,174,250,380,Redacted,Redacted
283,366,236,452,Redacted,Redacted
346,383,289,364,Redacted,Redacted
138,267,257,380,Redacted,Redacted
32,213,273,348,Redacted,Redacted
198,226,135,136,Redacted,Redacted
217,268,379,54,Redacted,Redacted
337,257,380,263,Redacted,Redacted
9,10,32,98,Redacted,Redacted
181,337,386,436,Redacted,Redacted
340,432,441,318,Redacted,Redacted
427,350,242,250,Redacted,Redacted
401,430,309,424,Redacted,Redacted
441,458,350,118,Redacted,Redacted
385,251,396,243,Redacted,Redacted
317,325,288,294,Redacted,Redacted
275,283,449,320,Redacted,Redacted
261,340,29,38,Redacted,Redacted
237,322,171,449,Redacted,Redacted
313,430,254,289,Redacted,Redacted
271,440,416,261,Redacted,Redacted
410,237,325,254,Redacted,Redacted
373,350,262,440,Redacted,Redacted
444,358,440,289,Redacted,Redacted
235,396,254,237,Redacted,Redacted
93,87,10,11,Redacted,Redacted
358,304,444,211,Redacted,Redacted
269,445,234,271,Redacted,Redacted"

let stop state =
  let open MentorPrefs in
  let mr = mean_rank state.choices in
  print_endline (stats_of_state state);
  print_string (string_of_state state);
  state.score > 0 && mr < 0.5
