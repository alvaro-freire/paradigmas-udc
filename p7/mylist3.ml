(* EJERCICIO 2 *)

let rec remove n list =
    match list with
    | [] -> []
    | hd::tl -> if (n = hd) 
                then tl 
                else hd::(remove n tl)
;;

let rec remove_all n list =
    match list with
    | [] -> []
    | hd::tl -> if (n = hd) 
                then remove_all n tl 
                else hd::(remove_all n tl)
;;

let rec ldif list_a list_b = 
    match list_b with
    | [] -> list_a
    | hd::tl -> ldif(remove_all hd list_a) tl
;;

let rec lprod list_a list_b = 
    match list_a with
    | [] -> []
    | hd::tl ->
        let rec aux h1 list_b = 
            match list_b with
            | [] -> []
            | hd::tl -> (h1, hd)::(aux h1 tl)
        in List.append (aux hd list_b) (lprod tl list_b)
;;

let rec divide list =
    match list with
    | h1::h2::tl -> let  list_a, list_b = divide tl 
                    in h1::list_a,h2::list_b
    | l -> l, []
;;