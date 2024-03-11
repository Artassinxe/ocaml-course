(* Echauffement *)
let hd = function
  | hd::_ -> hd 
  | [] -> failwith "Liste vide"
;;
let tl = function 
  | _::tl -> tl
  | [] -> failwith "Liste vide"
;;

(* Transposition d'une matrice *)
let rec tete liste = match liste with
  | [] -> []
  | l_hd::l_tl -> (hd l_hd :: tete l_tl)
;;

let rec reste liste = match liste with
  | [] -> []
  | hd_elem::tl_elem -> (tl hd_elem :: reste tl_elem)
;;

let rec trans liste = match liste with 
  | [] -> []
  | []::_ -> []
  | _ -> tete liste :: trans (reste liste)
;;

(* Fonctionnelle map *)

let rec map f = function
  | [] -> []
  | hd::tl -> (f hd)::map f tl
;;

let tete2 l = map (fun liste -> hd liste) l ;;

let reste2 l = map (fun liste -> tl liste) l ;;

let rec ligzero n = match n with
  | 0 -> []
  | _ -> 0::ligzero (n-1)
;;

let zero n = map (fun elem -> ligzero n) (ligzero n);;


let rec unite n = match n with
  | 0 -> [[]]
  | 1 -> [[1]]
  | _ -> (1::(ligzero (n-1)))::(map (fun liste -> 0::liste) (unite (n-1)))

let rec map2 f l1 l2 = match (l1,l2) with
  | ([],[]) -> []
  | ([],hd::tl) -> failwith "l2 > l1"
  | (hd::tl,[]) -> failwith "l1 > l2"
  | (hd1::tl1,hd2::tl2) -> (f hd1 hd2)::map2 f tl1 tl2
;;

let somlig l1 l2 = map2 (fun x1 x2 -> x1 + x2) l1 l2;;

let add m1 m2 = map2 somlig m1 m2;;

let rec prodligcol lig col = match (lig,col) with
| ([],[]) -> 0
| ([],hd::tl) -> failwith "ligne > colonne"
| (hd::tl,[]) -> failwith "colonne > ligne"
| (hd1::tl1,hd2::tl2) -> hd1 * hd2 + prodligcol tl1 tl2
;;

let rec prodligtmat1 mat lig = map (prodligcol lig) mat;;

let rec prod mat1 mat2 = map (prodligtmat1 mat2) mat1;;

let create1 f n = 
  let const = n+1 in
    let rec rec_create f c n = match n with
    | 0 -> []
    | _ -> f (c-n) :: rec_create f c (n-1)
  in rec_create f const n
;;

let rec create f n =
  match n with
  | 0 -> []
  | _ -> (create f (n - 1)) @ [f n]
;;

let couples n =
  let create_row i =
    create (fun j -> (i, j)) n
  in
  create create_row n
;;

let zero2 n =
  let create_row i =
    create (fun j -> 0) n
  in create create_row n
;;

let unite2 n = 
  let create_row i =
    create (fun j -> if (j=i) then 1 else 0) n
  in create create_row n
;;