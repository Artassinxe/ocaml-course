let filter pred l = 
  let l_output = [] in
  let rec filterRec pred l_in l_out = match l_in with
      | [] -> l_out
      | hd::ld -> 
        if (pred hd) then filterRec pred ld (hd::l_out)
        else filterRec pred ld l_out 
  in filterRec pred l l_output
;;

let rec exists pred = function
  | [] -> false
  | hd::ld -> 
    if (pred hd) then true
    else exists pred ld
  ;;
let rec for_all pred l = match l with
  | [] -> true
  | hd::ld -> 
    if not (pred hd) then false
    else for_all pred ld
  ;;
let for_all_v2 pred l = not (exists pred l);;


let rec mapFilter f pred = function
  | [] -> []
  | hd::tl -> 
    if (pred hd) then  (f hd) :: (mapFilter f pred tl)
    else (mapFilter f pred tl)
;;

let mapFilterTerm f pred l =
  let rec aux f pred acc = function
    | [] -> acc
    | hd::tl -> aux f pred (if (pred hd) then (f hd::acc) else acc) tl
  in aux f pred [] l
;;

type voiture_enr = {
  constructeur : string;
  modele : string;
  immatriculation : string;
  kilometrage : int;
  prix : int;
}

type voiture = V of string * string * string * int * int

let constructeur = function
  | V (c,_,_,_,_) -> c
;;

let model = function
  | V (_,m,_,_,_) -> m
;;

let immatriculation = function
  | V (_,_,i,_,_) -> i
;;

let kilometrage = function
  | V (_,_,_,k,_) -> k
;;

let prix = function
  | V (_,_,_,_,p) -> p
;;
let req1 x y l = 
  let pred elem = 
    let km = kilometrage elem in 
    x < km && km < y in
  filter pred l
;;


let req2 liste = 
  let renault = List.filter (fun e -> (constructeur e) = "Renault") liste in
  let v_filtrer = List.filter (fun x -> List.for_all (fun y -> x > y) renault) liste in
  List.map immatriculation v_filtrer
;;

(* Exercice 1
10
10
-2
-10
[4;3;6;1;2;3;7;8]
[7;8;4;3;6;1;2;3]
*)
