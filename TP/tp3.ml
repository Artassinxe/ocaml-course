let sum_sq l =
  let liste = List.map (fun x -> x*x) l in
  List.fold_left (fun a x -> x + a) 0 liste
;;

let sum_sq_trec l =
  let rec aux acc = function
    | [] -> acc
    | hd::tl -> aux (acc + hd*hd) tl
  in
  aux 0 l
;;

let inclus l1 l2 =
  List.for_all (fun x -> List.exists (fun i -> x = i) l2) l1
;;

let sup2 l1 l2 = List.for_all 
  (fun e -> List.exists (fun x -> List.exists (fun y -> x < e && e < y) l2) l2) l1
;; 

let sum l = List.fold_left (fun a x -> x + a) 0 l
;;

let pair_sup l = 
  (sum (List.filter (fun x -> (x mod 2) = 0) l)) 
  >
  (sum (List.filter (fun x -> (x mod 2) <> 0) l)) 
;;

let split l = 
  let rec aux couple = function
    | [] -> couple
    | hd::tl -> aux (match couple with (lp,li) -> if (hd mod 2 = 0) then (hd::lp,li) else (lp,hd::li)) tl
  in aux ([],[]) l
;;

let pair_sup_trec l = match (split l) with
  | (lp,li) -> sum lp + sum li
;;

let pair_sup_trec2 l = 
  let s = split l in
  let (lp,_) = s in 
  let (_,li) = s in (sum lp + sum li) ;;
;;

let rec f1 x = 0 + f1 x

let rec f2 n = f2 (0 + n)

let e1 = f1 5

let e2 = f2 5


let create n e = 
  let rec aux acc e n = match n with
    | 0 -> acc
    | _ -> aux (e::acc) e (n-1)
  in aux [] e n
;; 

let trie l =
  match l with | [] -> true | x::y ->
  let rec aux acc e = function
    | [] -> acc
    | hd::tl ->  aux (e < hd && acc) hd tl
  in aux true x y
;; 


let cat l1 l2 =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
in aux l2 (aux [] l1)
;;


let insert x l =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  let reverse l = aux [] l in
  let cat l1 l2 = aux l2 (reverse l1) in
  let rec insert_aux acc = function
    | [] -> reverse (x :: acc)
    | e :: tl ->
        if e > x then cat (reverse acc) (x :: e :: tl)
        else insert_aux (e :: acc) tl
  in
  insert_aux [] l
;;

let trier l = List.fold_left (fun acc e -> insert e acc) [] l;;