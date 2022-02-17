(* EJERCICIO 1 *)

let remove_all a l =
    let rec remove_all l1 = function
        | [] -> List.rev l1
        | h::t -> if h = a then remove_all l1 t
            else remove_all (h::l1) t
        in remove_all [] l
;;

let rec ldif l1 = function
    | [] -> l1
    | h::t -> ldif (remove_all h l1) t
;;


let onboard (m, n) (x, y) =
    (x >= 1) && (x <= m) &&
    (y >= 1) && (y <= n)
;;

let moves (x,y) = [
    (x-2, y-1);
    (x-2, y+1);
    (x-1, y-2);
    (x+1, y-2);
    (x-1, y+2);
    (x+1, y+2);
    (x+2, y-1);
    (x+2, y+1);
]

let valid_moves (m , n) (x , y) =
    let rec aux = function
        | [] -> []
        | h::t -> match onboard (m, n) h with
            | true -> h::aux t 
            | false -> aux t
    in aux (moves (x, y))
;;


let rec tour m n (x1, y1) (x2, y2) l =
    let mov = valid_moves (m, n) (x1, y1) in
    match List.mem (x2 , y2) mov with
        | true -> List.rev_append ((x1 , y1)::l) [(x2 , y2)]
        | false -> match (ldif mov l) with
            | [] -> raise Not_found
            | h::t -> let rec aux = function
                | [] -> raise Not_found
                | h::t -> try tour m n h (x2 , y2) ((x1 , y1)::l) with
                        Not_found -> aux t
    in aux (h::t);; 

let tour m n (x1 , y1) (x2 , y2) = tour m n (x1 , y1) (x2 , y2) [];;
