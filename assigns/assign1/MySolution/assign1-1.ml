(* ****** ****** *)

#use "./../assign1.ml";;

(* ****** ****** *)

(*
  Function to reverse the digits of an integer while preserving its sign.
  Parameters:
    - n: The integer to be reversed.
  Returns:
    - The integer with reversed digits.
*)
let intrev10 n =
  (* Define a recursive auxiliary function to reverse the digits. *)
  let rec aux acc n =
    if n = 0 then
      acc  (* If n is fully processed, return the reversed integer. *)
    else
      let digit = n mod 10 in  (* Extract the last digit of n. *)
      let new_acc = acc * 10 + digit in  (* Append the digit to the reversed result. *)
      aux new_acc (n / 10)  (* Recursively process the remaining digits. *)
  in
  if n < 0 then
    -aux 0 (-n)  (* If n is negative, reverse its absolute value and negate the result. *)
  else if n = 0 then
    0  (* If n is 0, return 0 as it remains unchanged when reversed. *)
  else
    aux 0 n  (* If n is positive, reverse its digits. *)
;;
