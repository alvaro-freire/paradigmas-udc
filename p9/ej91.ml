(* EJERCICIO 1 *)

let rec to0from n list =
	if n < 0 then List.rev list
	else to0from (n-1) (n::list)
;;
	
let to0from n = to0from n [];;

let rec fromto m n list =
	if m > n then List.rev list
	else fromto (m + 1) n (m::list)
;;

let fromto m n = fromto m n [];;
	
let rec from1to n list = 
	if n < 1 then list
	else from1to (n - 1) (n :: list)
;;

let from1to n = from1to n [];;

let rec map f list lt =
    match list with
    | [] -> List.rev lt
    | hd::tl -> map f tl ((f hd)::lt)
;;

let map f list = map f list [];;

let rec power x y a =
	if y < 0 then invalid_arg "power"
	else if y = 0 then a
	else power x (y - 1) (x * a)
;;

let power x y = power x y 1;;

let rec incseg list lt r = match list with
	| [] -> List.rev lt
	| hd::tl -> incseg tl ((hd + r)::lt) (hd + r)
;;

let incseg list = incseg list [] 0;;

let rec remove x l1 l2 = 
    match l1 with
        | [] -> []
        | h::t -> if h = x then List.rev_append l2 t
                  else remove x t (h::l2)
;;

let remove x l = remove x l [];;

let rec divide list impar par n = match list with
	| [] -> (List.rev impar, List.rev par)
	| hd::tl -> if (n mod 2 = 0) then divide tl impar (hd::par) (n + 1)
				     else divide tl (hd::impar) par (n + 1)
;;

let divide list = divide list [] [] 1;;

let rec compress list lt = match list with
	| [] -> lt
	| h1::h2::tl -> if (h1 = h2) then compress (h1::tl) lt
			             else compress (h2::tl) (h1::lt)
	| hd::tl -> List.rev (hd::lt)
;;

let compress list = compress list [];;
