let is_prime n =
	let rec check_from i =
		i >= n ||
		(n mod i <> 0 && check_from (i+1))
		in check_from 2;;

let is_prime2 n =
    	let n = abs n
	in let rec is_not_divisor d =
		d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
		in n <> 1 && is_not_divisor 2;;	

let rec next_prime n =
	if is_prime(n + 1) then
		n + 1 else next_prime(n + 1);;

let rec last_prime_to n =
	if is_prime(n) then
        n else last_prime_to(n - 1);;
