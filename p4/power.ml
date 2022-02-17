(* EJERCICIO 1 *)

let rec power x y = 
	if x = 0 then 0		(* casos *)
	else if y = 0 then 1    (* base  *)
	else x * power x (y-1);;

(* Propiedad 1: x^y = (x * x)^(y / 2)     , si y es par   *)

(*
	(x * x)^(y / 2) = (x^2)^(y / 2) =
      = x^(2 * (y / 2)) = x^y

EJEMPLO: 2^4 = (2 * 2)^(4 / 2) =
             = 2^(2 * 4 / 2)   =
             = 2^4
*)


(* Propiedad 2: x^y = x * (x * x)^(y / 2) , si y es impar *)

(* 
	[ x es impar => x / 2 = (x - 1) / 2 ] // trabajando con enteros

	x * (x * x)^(y / 2) = x * (x^2)^(y / 2) =
      = x * (x^(2 * (y - 1) / 2) = x * x^(y - 1) 

EJEMPLO: 2^5 = 2 * (2 * 2)^(5 / 2) =
	     = 2 * (2 * 2)^(4 / 2) =
             = 2 * 2^4
*)

let rec power' x y =
	if x = 0 then 0         (* casos *)
        else if y = 0 then 1    (* base  *)
	else if (y mod 2 = 0) then power' (x*x) (y/2)
	else x * power' (x*x) ((y - 1)/2);;

(* ---------------------------------------------------------------------- *)
(*	la funcion power' es más eficiente porque para power hay un       *)
(*	total de 'y' llamadas, mientras que para power habría log2(y)     *)
(* ---------------------------------------------------------------------- *)

let rec powerf x n =
	if x = 0. then 0.          (* casos *)
        else if n = 0  then 1.     (* base  *)
        else if (n mod 2 = 0) then powerf (x*.x) (n/2)
        else x *. powerf (x*.x) ((n - 1)/2);;
