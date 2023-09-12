(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let isPrime n0 =
  if n0 <= 1 then
    false  (* 0 and 1 are not prime *)
  else
    let rec is_prime_helper n d =
      if d * d > n then
        true  (* No divisors found, it's prime *)
      else if n mod d = 0 then
        false  (* Found a divisor, it's not prime *)
      else
        is_prime_helper n (d + 1)
    in
    is_prime_helper n0 2  (* Start checking divisors from 2 *)
;;