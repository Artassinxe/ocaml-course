(* EXERCICE REVISION + RECURSIVITE TERMINALE *)

let f2 l = List.map (fun x -> (x,x)) l;;
(* C'est une Eta-expansion *)

(* fold_right :  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
(* fold_left :  ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)

let fact n =
  let rec aux acc = function
    | 0 -> acc
    | 1 -> acc
    | n -> aux (acc*n) (n-1)
  in aux 1 n 
;; 

let rec reverse = function 
  | [] -> []
  | hd::tl -> (reverse tl) @ [hd]

let reverse list =
  let rec aux acc list = match list with
    | [] -> acc
    | hd::tl -> aux (hd::acc) tl
  in aux [] list
;;


(* [1;2;3] *)
(* EXERCICE 1 *)




let rec occs = 
  let rec inserer x = function
    |[] -> [(x,1)]
    |(y,n) :: l -> if x=y then (y,n+1) :: l
        else (y,n) :: inserer x l
  in function 
    |[]->[]
    |x :: l -> inserer x (occs l)
;;

let occs1 l = 
  let rec insert e = function
    | [] -> [(e,1)]
    | (elem,occ)::tl -> 
      if elem = e then (elem,occ+1)::tl
      else (elem,occ)::(insert e tl)
  in let rec occs_tail_r l_out = function
    | [] -> l_out
    | hd::tl -> occs_tail_r (insert hd l_out) tl
  in occs_tail_r [] l
;;

let occs l = 
  let rec insert e = function
  | [] -> [(e,1)]
  | (elem,occ)::tl -> 
    if elem = e then (elem,occ+1)::tl
    else (elem,occ)::(insert e tl)
  in
  List.fold_right (fun e acc -> insert e acc) l []
;;

let doublons l = 
  List.fold_left 
  (fun acc e -> 
    if (List.exists (fun x -> x = e) acc) then acc 
    else e::acc) 
  [] l