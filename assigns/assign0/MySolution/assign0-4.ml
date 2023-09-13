(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)


let str2int cs =
  let rec aux acc index =
    if index < 0 then
      acc
    else
      let digit = Char.code cs.[index] - Char.code '0' in
      let new_acc = acc + (digit * int_of_float (10.0 ** float_of_int (String.length cs - index - 1))) in
      aux new_acc (index - 1)
  in
  if String.length cs = 0 then
    0
  else
    aux 0 (String.length cs - 1)
;;