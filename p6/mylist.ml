(* EJERCICIO 1 *)

let hd list =
    match list with
        | [] -> failwith "empty list"
        | [hd] -> hd
        | hd::tl -> hd
;; 

let tl list =
    match list with
        | [] -> failwith "empty list"
        | [tl] -> [tl]
        | hd::tl -> tl
;;

let rec length list =
    match list with
        | [] -> 0
        | [a] -> 1
        | hd::tl -> length tl + 1
;;

let compare_lengths list_a list_b =
    if (length list_a > length list_b) then 1
    else if (length list_a = length list_b) then 0
    else -1
;;

let rec nth list n =
    if (length list - 1 < n) then failwith "not enough items in list"
    else if (n = 0) then (
        match list with
            | [] -> failwith "empty list"
            | [hd] -> hd
            | hd::tl -> hd
    ) else (
        match list with
        | [] -> failwith "empty list"
        | [hd] -> hd
        | hd::tl -> nth tl (n - 1)
    )
;;

let rec append list_a list_b =
    match list_a with
    | [] -> list_b
    | hd::tl -> hd::(append tl list_b)

;;

let rec find f list =
    match list with
    | [] -> failwith "Not Found" 
    | hd::tl -> if (f hd) then hd else find f tl

;;

let rec for_all f list =
    match list with
    | [] -> true
    | hd::tl -> if (f hd) then for_all f tl else false

;;

let rec exists f list =
    match list with
    | [] -> true
    | hd::tl -> if (f hd) then true else exists f tl

;;


let rec mem a list =
    match list with
    | [] -> false
    | hd::tl -> if (hd = a) then true else mem a tl

;;

let rec filter f list =
    match list with
    | [] -> []
    | hd::tl -> if (f hd) then hd::(filter f tl)
                else filter f tl

;;

let rec find_all f list =
    match list with
    | [] -> []
    | hd::tl -> if (f hd) then hd::(find_all f tl)
                else find_all f tl
;;

let rec partition f list (l1, l2)=
    match list with
    | [] -> (l1, l2)
    | hd::tl -> if (f hd) then partition f tl (append l1 [hd], l2)
                else partition f tl (l1, append l2 [hd])
                
;;

let partition f list = partition f list ([], []);;

let rec split list (l1, l2) = match list with
	| [] -> (l1, l2)
	| (a, b)::tl -> split tl (append l1 [a], append l2 [b])

;;

let split list = split list ([], []);;

let rec combine l1 l2 list =
    if (length l1 <> length l2) then failwith "Invalid_argument"
    else (
        match l1 with
        | [] -> list
        | hd1::tl1 -> match l2 with
                    | [] -> list
                    | hd2::tl2 -> 
                        combine tl1 tl2 (append list [(hd1, hd2)])
    )

;;

let combine l1 l2 = combine l1 l2 [];;

let rec init len f (list)=
    if (len < 0) then failwith "Invalid_argument"
    else (
        match len with
        | 1 -> (append [f (len-1)] list)
        | _ -> init (len - 1) f (append [f (len-1)] list)
    )

;;

let init len f = init len f [];;

let rec rev list (revlist) =
    match list with
    | [] -> revlist
    | hd::tl -> rev tl (hd::revlist)

;;

let rev list = rev list [];;

let rec rev_append l1 l2 =
    match l1 with
    | [] -> l2
    | hd::tl -> rev_append tl (hd::l2)

;;

let rec concat list_i (list_o) =
    match list_i with
    | [] -> list_o
    | hd::tl -> concat tl (append list_o hd)
    
;;

let concat list_i = concat list_i [];;

let rec flatten list_i (list_o) =
    match list_i with
    | [] -> list_o
    | hd::tl -> flatten tl (append list_o hd)
    
;;

let flatten list_i = flatten list_i [];;


let rec map f list (list_ret) =
    match list with
    | [] -> list_ret
    | hd::tl -> map f tl (append list_ret [f hd])

;;

let map f list = map f list [];;

let rec rev_map f list (list_ret) =
    match list with
    | [] -> list_ret
    | hd::tl -> rev_map f tl ((f hd)::list_ret)

;;

let rev_map f list = rev_map f list [];;

let rec map2 f l1 l2 list =
    if (length l1 <> length l2) then failwith "Invalid_argument"
    else (
        match l1 with
        | [] -> list
        | hd1::tl1 -> match l2 with
                    | [] -> list
                    | hd2::tl2 -> 
                        map2 f tl1 tl2 (append list [(f hd1 hd2)])
    )
;;

let map2 f l1 l2 = map2 f l1 l2 [];;

let rec fold_left f init list =
    match list with
    | [] -> init
    | hd::tl -> fold_left f (f init hd) tl
;;

let rec fold_right f list init =
    match list with
    | [] -> init
    | hd::tl -> f hd (fold_right f tl init)
;;
