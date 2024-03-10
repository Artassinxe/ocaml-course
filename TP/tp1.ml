(* Prélude - pas besoin de recopier ce code *)
type binop =
  | And (* conjonction booléenne (&&) *)
  | Or  (* disjonction booléenne (||) *)
  | Add (* addition des nombres entiers (+) *)
  | Leq (* comparaison (<=) sur les entiers *)
type typ = Int | Bool
type expr =
  | IConst of int  (* constante entière *)
  | BConst of bool (* constante booléenne *)
  | Var of string  (* variable définie par son identificateur *)
  | If of expr * expr * expr (* If(b,e1,e2) <=> "if b then e1 else e2" *)
  | Let of string * expr * expr (* Let(x,e1,e2) <=> "let x = e1 in e2" *)
  | Call of binop * expr * expr (* Call(Add,e1,e2) <=> "e1 + e2", etc. *)
module type tENV_LIST = sig
  type ('x, 'v) env = ('x * 'v) list
  val empty : ('x, 'v) env
  val get : 'x -> ('x, 'v) env -> 'v
  val put : 'x -> 'v -> ('x, 'v) env -> ('x, 'v) env
end
module type tENV_FUN = sig
  type ('x, 'v) env = 'x -> 'v
  val empty : ('x, 'v) env
  val get : 'x -> ('x, 'v) env -> 'v
  val put : 'x -> 'v -> ('x, 'v) env -> ('x, 'v) env
end
module type tENV = sig
  type ('x, 'v) env
  val empty : ('x, 'v) env
  val get : 'x -> ('x, 'v) env -> 'v
  val put : 'x -> 'v -> ('x, 'v) env -> ('x, 'v) env
end
module type tEVAL = functor (E : tENV) -> sig
    type value = Vint of int | Vbool of bool
    val eval_op : binop -> value -> value -> value
    val eval : (string, value) E.env -> expr -> value
end
module type tTYPECHECK = functor (E : tENV) -> sig
  val typeof_op : binop -> typ * typ * typ
  val typeof : (string, typ) E.env -> expr -> typ
end

(* Début du TP *)


let doublons liste =
  let rec test temp result = match temp with
    | head::tail -> 
      if (List.mem head tail) && not (List.mem head result) then test tail (head :: result)
      else test tail result
    | [] -> result
  in
  test liste []


  let occs liste =
    let doublonListe = doublons liste in
    let rec countOcc elem = function
    | head::tail -> 
      if elem = head then 1 + countOcc elem tail
      else countOcc elem tail
    | [] -> 0
    in
    let rec test doublonListe liste result = match doublonListe with
      | head::tail -> let occurrence = countOcc head liste in test tail liste ((head, occurrence) :: result)
      | [] -> result
    in
    test doublonListe liste []
  
module ENV_LIST : tENV_LIST = struct
  type ('x, 'v) env = ('x * 'v) list
  let empty : ('x,'v) env = []
  let rec get ident = function
    | [] -> failwith "Pas de valeur trouvée" 
    | (x,v)::tail -> 
      if x = ident then v
      else get ident tail
  let put ident value liste =
    let rec aux = function
      | [] -> (ident, value)::liste
      | (x, v)::tail -> 
          if x = ident then liste
          else (x, v)::aux tail
    in aux liste
end

module ENV_FUN : tENV_FUN = struct
  type ('x, 'v) env = 'x -> 'v

  let empty : ('x, 'v) env = fun _ -> failwith "Environnement vide"

  let put x v env =
    fun y -> if y = x then v else env y

  let get x env = env x
end

let regle_if expr = match expr with
  | If (Var e1,BConst true, BConst false) -> Var e1
  | If (Var e1,BConst true, Var e2) -> Call(Or,Var e1,Var e2)
  | If (Var e1,Var e2, BConst false) -> Call(And,Var e1,Var e2)
  | _ -> expr

let rec apply regle expr =
  let expression = regle expr in
  match expression with
  | If(c, e1, e2) ->
      let a = apply regle e1 in
      let b = apply regle e2 in
      regle (If(c, a, b))
  | Let(x, e1, e2) ->
      let a = apply regle e1 in
      let b = apply regle e2 in
      regle (Let(x, a, b))
  | Call(op, e1, e2) ->
      let a = apply regle e1 in
      let b = apply regle e2 in 
      regle (Call(op, a, b))
  | _ -> expression  


module EVAL : tEVAL = functor (E : tENV) -> struct
  type value = Vint of int | Vbool of bool
  let eval_op binop v1 v2 = match (v1,v2) with
    | (Vint a,Vint b) -> 
        if (binop = Add) then Vint (a + b)
        else if (binop = Leq) then Vbool (a <= b)
        else failwith "Vint : Opperation incompatible"
    | (Vbool a,Vbool b) -> 
        if (binop = And) then Vbool (a && b)
        else if (binop = Or) then Vbool (a || b)
        else failwith "Vbool : Opperation incompatible"
    | _ -> failwith "Constructeurs differents"
  let rec eval env expr = match expr with
    | BConst const -> Vbool const
    | IConst const -> Vint const
    | Let (ident,e1,e2) -> let a = (eval env e1) in eval (E.put ident a env) e2
    | Var ident -> (E.get ident env)
    | If (e1,e2,e3) -> (match (eval env e1) with
      | Vint i -> if (i <> 0) then (eval env e2) else (eval env e3)
      | Vbool i -> if i then (eval env e2) else (eval env e3))
    | Call (op,e1,e2) -> 
      let a = eval env e1 in
      let b = eval env e2 in eval_op op a b
end


module TYPECHECK : tTYPECHECK = functor (E : tENV) -> struct
  let typeof_op = function
    | Add -> (Int,Int,Int)
    | Leq -> (Int,Int,Bool)
    | _ -> (Bool,Bool,Bool)
  let rec typeof env = function
    | BConst _ -> Bool
    | IConst _ -> Int
    | Call (op, arg1, arg2) ->
        let (_, _, tresult) = typeof_op op in
        let targ1 = typeof env arg1 in
        let targ2 = typeof env arg2 in
        (match (targ1, targ2) with
        | (Bool, Bool) when op = And || op = Or -> tresult
        | (Int, Int) when op = Add || op = Leq -> tresult
        | _ -> failwith "Types d'opérandes incompatibles")
    | Var ident -> E.get ident env
    | Let (ident, e1, e2) ->
        let t1 = typeof env e1 in
        typeof (E.put ident t1 env) e2
    | _ -> failwith "Expression non prise en charge"
end