(* EJERCICIO 1 *)

let rec qsort1 ord = function
    | [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              qsort1 ord before @ h :: qsort1 ord after
;;

(*
    * Respuesta 1:
    
            Esta implementación no tendrá un buen rendimiento con listas
        de gran tamaño debido a que no es una función recursiva terminal
        y podría darse agotamiento de memoria (stack overflow).
*)

let rec qsort2 ord =
    let append' l1 l2 = List.rev_append (List.rev l1) l2 in
    function
        | [] -> []
        | h::t -> let after, before = List.partition (ord h) t in
                  append' (qsort2 ord before) (h :: qsort2 ord after)
;;

(*
    * Respuesta 2:

            Si, sí tiene ventaja qsort2, esta función admite listas de tamaños
        mayores. En qsort1, las llamadas recursivas que ordenan las sublistas 
        son unidas con '@', lo que implica una no terminalidad. Con qsort2, 
        utilizamos una unión de listas que sí está implementada de forma recursiva 
        terminal, por lo que podremos ahorrar un poco de espacio en el stack en
        las llamadas recursivas, aunque este cambio todavía no implique la 
        terminalidad de qsort2.

            Lista de ejemplo: let list = List.init 1000000 (fun _ -> Random.int 1000);;

            Esta lista de enteros tiene un tamaño de 1.000.000 elementos, con valores
        entre 0 y 999. Si intentamos ordenarla con qsort1 obtenemos el mensaje de error
        "Stack overflow during evaluation (looping recursion?).", mientras que qsort2
        sí consigue ordenarla correctamente. 

    * Respuesta 3:

            La desventaja de qsort2 es que es un poco más lenta que qsort1,
        debido a que para evitar la no terminalidad con la variable append', 
        se utiliza el List.rev_append, que invirte la lista dos veces, para 
        seguir conservándola en su orden correcto. Además, dicha variable 
        está declarada localmente y se declara a cada llamada recursiva a 
        qsort2, cuando debería estar declarada de manera global para evitar 
        este uso innecesario de CPU, que se traduce en un tiempo de ejecución
        mayor. 
     
            El porcentaje aproximado de penalización por esto es del 10% con
        respecto a qsort1 (con una lista de tamaño 200_000 y enteros del 0 al
        99, se obtuvieron tiempos de ~22 segundos frente a 20).

*)
