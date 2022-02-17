(* EJERCICIO 2 *)

let rec mcd (x, y) =
	if y = 0 then x
	else mcd (y, (x mod y));;
