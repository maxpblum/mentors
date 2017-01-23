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
