(* ****** ****** *)

#use "./../assign1.ml";;

(* ****** ****** *)

let intrev10 n =
  let rec aux acc n = 
    if n = 0 then
      acc
    else 
      let digit = n mod 10 in
      let new_acc = acc * 10 + digit in
      aux new_acc (n / 10)
  in
  if n < 0 then
    -aux 0 (-n) 
  else if n = 0 then
    0
  else 
    aux 0 n
;;
