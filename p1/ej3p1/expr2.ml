(* 3.1 - Un valor u de tipo int a partir de una expresión
 que contenga, al menos, 4 operadores infijos. *)
let u = 4 + 2 - 4 / 2;;
(* 3.2 - Un valor v de tipo float a partir de una expresión
 que incluya una función predefinida. *)
let v = 2. *. asin 1.;;
(* 3.3 - Un valor w de tipo char a partir de una expresión
 que incluya una sub-expresión de tipo int. *)
let w = char_of_int u;;
(* 3.4 - Un valor x de tipo bool a partir de una expresión
 que incluya una o más funciones u operadores. *)
let x = (3 < u || int_of_float v > 4);;
(* 3.5 - Un valor y de tipo string a partir de una expresión
 que contenga una frase if-then-else. *)
let y = if x then "true" else "false";;

