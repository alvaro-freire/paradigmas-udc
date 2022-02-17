let rec fib n =
    if n <= 1 then n
    else fib (n-1) + fib (n-2);;

let rec to_fib n =
    if n > int_of_string(Sys.argv.(1)) then (exit 0)
    else print_endline(string_of_int(fib n));
         to_fib (n + 1);;

(function true -> to_fib 0 | false -> print_endline("fib: número de argumentos inválido"))
	(Array.length(Sys.argv) = 2)

