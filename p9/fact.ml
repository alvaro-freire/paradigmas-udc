(* EJERCICIO 3 *)

let rec fact = function
    0 -> 1
    | n -> n * fact (n - 1)
;;

if Array.length Sys.argv != 2 then 
begin
    Printf.printf "fact: número de argumentos inválido\n";
    exit 1;
end;;

try 
    Printf.printf "%i\n" (fact (int_of_string Sys.argv.(1)))
with exc -> Printf.printf "fact: argumento inválido\n";;
