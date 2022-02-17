(* EJERCICIO 4 *)

false && (2 / 0 > 0);;

(* - : bool = false *)

true && (2 / 0 > 0);;

(* Exception: Division_by_zero. *)

true || (2 / 0 > 0);;

(* - : bool = true *)

false || (2 / 0 > 0);;

(* Exception: Division_by_zero. *)

let con b1 b2 = b1 && b2;;

(* val con : bool -> bool -> bool = <fun> *)

let dis b1 b2 = b1 || b2;;

(* val con : bool -> bool -> bool = <fun> *)

con (1 < 0) (2 / 0 > 0);;

(* Exception: Division_by_zero. *)

(1 < 0) && (2 / 0 > 0);;

(* - : bool = false *)

dis (1 > 0) (2 / 0 > 0);;

(* Exception: Division_by_zero. *)

(1 > 0) || (2 / 0 > 0);;

(* - : bool = true *)
