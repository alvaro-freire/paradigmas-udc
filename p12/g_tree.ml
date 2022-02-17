type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    | Gt (_, []) -> 1
    | Gt (r, h::t) -> size h + size (Gt (r,t))
;;

let rec height = function 
    | Gt (_, []) -> 1
    | Gt (r, h::t) -> max (1 + height h) (height (Gt (r, t)))
;;

let rec leaves = function 
    | Gt (n, []) -> [n]
    | Gt (r, h::t) -> if t = [] then leaves h 
                      else (leaves h) @ (leaves (Gt (r, t)))
;;

let rec mirror (Gt (r, l)) =
    Gt(r, List.rev(List.map mirror l));;

let rec preorder = function 
    | Gt (r, []) -> [r]
    | Gt (r, Gt (r1, l1) :: t) -> r :: preorder (Gt (r1, l1 @ t))
;;

let rec postorder = function
    | Gt (n, []) -> [n]
    | Gt (r, h::t) -> postorder h @ postorder (Gt (r, t))
;;