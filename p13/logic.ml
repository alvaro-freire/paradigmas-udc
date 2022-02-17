(* EJERCICIO 1 *)

type log_exp =
    | Const of bool
    | Var of string
    | Neg of log_exp
    | Disj of log_exp * log_exp
    | Conj of log_exp * log_exp
    | Cond of log_exp * log_exp
    | BiCond of log_exp * log_exp
;;

let rec eval ctx = function
    | Const b -> b
    | Var s -> List.assoc s ctx
    | Neg e -> not (eval ctx e)
    | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
    | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
    | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
    | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2)
;;

type oper = Not;;

type biOper = Or | And | If | Iff;;

type prop =
    | C of bool
    | V of string
    | Op of oper * prop
    | BiOp of biOper * prop * prop
;;


(* 1. a) *)

let rec prop_of_log_exp = function
    | Const b -> C b
    | Var s -> V s
    | Neg e -> Op (Not, prop_of_log_exp (e))
    | Disj (e1, e2) -> BiOp (Or, (prop_of_log_exp e1), (prop_of_log_exp e2))
    | Conj (e1, e2) -> BiOp (And, (prop_of_log_exp e1), (prop_of_log_exp e2))
    | Cond (e1, e2) -> BiOp (If, (prop_of_log_exp e1), (prop_of_log_exp e2))
    | BiCond (e1, e2) -> BiOp (Iff, (prop_of_log_exp e1), (prop_of_log_exp e2))
;;

let rec log_exp_of_prop = function
    | C b -> Const b 
    | V s -> Var s
    | Op (Not, e) -> Neg (log_exp_of_prop e) 
    | BiOp (biOper, e1, e2)-> 
                    match biOper with
                    | Or -> Disj ((log_exp_of_prop e1), (log_exp_of_prop e2))
                    | And -> Conj ((log_exp_of_prop e1), (log_exp_of_prop e2))
                    | If -> Cond ((log_exp_of_prop e1), (log_exp_of_prop e2))
                    | Iff -> BiCond ((log_exp_of_prop e1), (log_exp_of_prop e2))
;;


(* 1. b) *)

let opval = function
    | Not -> not
;;

let biopval = function
    | Or -> (||)
    | And -> (&&)
    | If -> (fun x y -> not x || y)
    | Iff -> (=)
;;

let rec peval ctx = function
    | C b -> b
    | V s -> List.assoc s ctx
    | Op (oper, e) -> opval oper (peval ctx e)
    | BiOp (biOper, e1, e2) -> biopval biOper (peval ctx e1) (peval ctx e2)
;;


(* 1. c) *)

let rec variables = function
    | C b -> []
    | V s -> [s]
    | Op (oper, p) -> variables p
    | BiOp (op, p1, p2) -> 
                        let ev1 = variables p1 and ev2 = variables p2
                        in List.filter (function x -> not(List.mem x ev2)) ev1 @ ev2 
;;

let rec contexto ctx p = function
    | [] -> peval ctx p 
    | h::t -> contexto ((h, true)::ctx) p t && contexto ((h, false)::ctx) p t
;;

let rec is_tau p = let va = variables p in contexto [] p va;;