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

let mapFilter f pred l = 
  let l_output = [] in
  let rec map_filter_rec f pred l_in l_out = match l_in with
    | [] ->  l_out
    | hd::ld ->
      if (pred hd) then map_filter_rec f pred ld (f hd::l_out)
      else map_filter_rec f pred ld l_out
    in map_filter_rec f pred l l_output
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

let constructeur = function
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

let req2 x l = 
  let pred elem = kilometrage elem > x in 
  mapFilter (fun elem -> immatriculation elem) pred l

(* à finir *)