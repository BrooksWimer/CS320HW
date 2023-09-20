(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)


let str2int cs =
  let rec aux acc index position_val =
    if index < 0 then
      acc
    else
      let digit = ord cs.[index] - ord '0' in
      let new_acc = acc + (digit * position_val) in
      aux new_acc (index - 1) (position_val*10)
  in
  if string_length cs = 0 then
    0
  else
    aux 0 (string_length cs - 1) (1)
;;