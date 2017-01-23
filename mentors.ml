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
