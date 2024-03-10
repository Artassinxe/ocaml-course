type couleur = 
  | Bleu
  | Blanc
  | Rouge
;;

type motif = 
  | Roi
  | Dame
  | Valet
  | Num of int
;;

let value = function
  | n when n >= 1 && n <=10 -> Num n
  | _ -> failwith "La valeur est incorrect"
;;

type carte = {motif : motif; couleur : couleur};;

let valet = {motif = Valet; couleur = Rouge};;

let dix = {motif = value 10; couleur = Rouge};;

type jour = 
  |Lu
  |Ma
  |Me
  |Je
  |Ve
  |Sa
  |Di
;;

type nombre = Entier of int | Reel of float | Complex of float * float;;

type result = Int of int | Failure;;

type 'param option = None | Param of 'param;;

type etat = State of int;;

type symbole = Symbole of string;;

type nfa = Nfa of etat * (etat -> symbole -> etat list) * bool;;

type monAutomate = nfa;;

let z = Complex (1.,2.);;

let r = Reel 2.;;

(* Type récursif *)
type 'a ab = Feuille of 'a | Noeud of ('a ab * 'a ab);;

type 'a arbreNaire = Node of 'a * 'a arbreNaire list;;

type action = Tourner of int | Avancer of int | Lever of bool | Sequence of action * action | Repetition of int * action;;

let t = Noeud (Feuille 4, Noeud (Feuille 5, Feuille 6));;
(* Type de t : int ab *)
let t6 = Node (1, [Leef 2; Node(3,[Leef 5;Leef 6]) ;Leef 4]);;
(* Type de t6 : int arbreNaire *)

let carre = Repetition (4 , Sequence (Avancer 10, Tourner 90))


(* PARTIE 2 : Filtrage et récursion pour manipuler des objets de type utilisateur *)

let colorAssociate = 
  fun couleur -> match couleur with
  | Rouge -> 1
  | Bleu -> 2
  | Blanc -> 3
;;


let lendemain j =
  let listeJour = [Lu;Ma;Me;Je;Ve;Sa;Di] in
  let rec lendemainRec j = function
    | [] -> failwith "Pas de jour"
    | _::[] -> Lu
    | hd::ld::_ when hd = j -> ld
    | _::reste -> lendemainRec j reste
  in lendemainRec j listeJour
;;

let somme liste_carte =
  let sum = 0 in
  let rec somme_rec s = function
    | [] -> s
    | Num nb::reste -> somme_rec (s+nb) reste
    | _::reste -> somme_rec (s+10) reste
  in somme_rec sum liste_carte
;;

let addition_nombres nb1 nb2 =
  match (nb1, nb2) with
  | (Entier x, Entier y) -> Entier (x + y)
  | (Reel x, Reel y) -> Reel (x +. y)
  | (Complex (a, b), Complex (c, d)) -> Complex (a +. c, b +. d)
  | _ -> failwith "Impossible d'additionner des nombres de types différents"



let abEquality arbre1 arbre2 =
  let rec test (ab1, ab2) = match (ab1, ab2) with
    | (Feuille a, Feuille b) -> a = b
    | (Noeud (a1, a2), Noeud (b1, b2)) -> test (a1, b1) && test (a2, b2)
    | _ -> false
  in
  test (arbre1, arbre2)


let abEqualityModified arbre1 arbre2 =
  let rec test (ab1, ab2) = match (ab1, ab2) with
    | (Feuille a, Feuille b) -> a = b
    | (Noeud (a1, a2), Noeud (b1, b2)) -> test (a1, b1) || test (a2, b2)
    | _ -> false
  in
  test (arbre1, arbre2)

let prefix f =
  let rec prefix_rec = function
    | Leef elem -> f elem
    | Node (elem, liste) -> f elem; prefix_liste liste
  and prefix_liste = function
    | head :: reste -> prefix_rec head; prefix_liste reste
    | [] -> ()
  in
  prefix_rec
  

  let actionFollow action =
    let etat = false in 
    let rec actionFollowRec etat action = match action with
    | Lever state -> state
    | Sequence (act1,act2) -> actionFollowRec etat act1 && actionFollowRec etat act2
    | _-> etat
  in actionFollowRec etat action
