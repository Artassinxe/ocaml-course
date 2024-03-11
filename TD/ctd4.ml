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

let reverse list =
  let rec aux acc = function
    | [] -> acc
    | hd::tl -> aux (hd::acc) tl
  in aux [] list
;; 

(* EXERCICE 1 *)