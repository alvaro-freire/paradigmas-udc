(* EJERCICIO 2 *)

(* original: *)
let rec divide l = 
    match l with
    | h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
    | _ -> l, []
;;

(* redef.: *)

let rec divide' list impar par n = 
    match list with
    | [] -> (List.rev impar, List.rev par)
    | h::t -> if (n mod 2 = 0) 
              then divide' t impar (h::par) (n + 1)
              else divide' t (h::impar) par (n + 1)
;;

let divide' list = divide' list [] [] 1;;


(* =============================================================================== *)

(* redef. (1): *)
let rec merge ord = function
    | [], l | l, [] -> l
    | h1::t1, h2::t2 -> if ord h1 h2 then h1 :: merge ord (t1, h2::t2)
                        else h2 :: merge ord (h1::t1, t2)
;;

(* redef. (2): *)
let rec merge' ord (l1, l2) l = 
    match (l1, l2) with
    | [], list | list, [] -> List.rev_append l list
    | h1::t1, h2::t2 -> if ord h1 h2 then merge' ord (t1, h2::t2) (h1::l)
                        else merge' ord (h1::t1, t2) (h2::l)
;;

let merge' ord (l1, l2) = merge' ord (l1, l2) [];;

(* =============================================================================== *)

(* redef.: *)
let rec msort1 ord l =
    match l with
    | [] | _::[] -> l
    | _ -> let l1, l2 = divide l in
           merge ord (msort1 ord l1, msort1 ord l2)
;;

(* =============================================================================== *)

let rec msort2 ord l =
    match l with
    | [] | _::[] -> l
    | _ -> let l1, l2 = divide' l in
           merge' ord (msort2 ord l1, msort2 ord l2)
;;


(* 
    * Pregunta: ¿Puede provocar algún problema la no terminalidad de divide o merge?
 
        Sí, porque al no ser recursivas terminales provocan que msort1 no pueda
    manejar trabajar con listas de gran tamaño, debido a un agotamiento de memoria
    (stack overflow) antes de que pueda terminar.

*)
    
   (* Ejemplo de lista: *) let l2 = List.init 500_000 (fun _ -> Random.int 100);;

(*
    * Comparación de tiempos de ejecución:

         Se definió una lista de tamaño 90_000 con valores aleatorios entre 0 y 99.
     Los tiempos de ejecución fueron los siguientes:

     * qsort2: 4.5154 s
     * msort1: 0.1717 s
     * msort2: 0.2298 s

    - msort1: Es el algoritmo que más limitaciones tiene a la hora de
             manejar listas de gran tamaño, pero el más rápido de los tres.

    - qsort2: Puede manejar listas de tamaños mayores que msort1, pero su
             tiempo de ejecución es el más lento.

    - msort2: El tiempo de ejecución de este algoritmo es casi tan bajo como
             el msort1 y tiene la ventaja de que puede trabajar con listas
             mucho más grandes que los otros dos algoritmos.
*)
