(* EJERCICIO 1 *)

let f n = 
	if n mod 2 = 0 
	then n / 2 
	else 3 * n + 1;;

let rec orbit n =
	if (n = 1) then (
	print_string(string_of_int(n)); print_newline(); ()
	) else if (f n = 1) then (
	print_string(string_of_int(n) ^ ", " ^ "1");
	print_newline(); ()
	) else (
	print_string(string_of_int(n) ^ ", "); orbit (f n); ()
	);;

let rec length n (l) =
	if (n = 1)
	then l
	else length (f n) (l + 1)

let length n = length n (0);;

let rec top n (t) =
	if (n = 1)
	then t
	else (top (f n) (max n t))
	
let top n = top n (1);;
	

let rec length'n'top n (l, t) =
	if (n = 1) then
	(l, t)
	else length'n'top (f n) (l + 1, max n t)
	
let length'n'top n = length'n'top n (0, 1);;