(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let stringrev(cs: string): string =
  let len = string_length cs in
  string_init len (fun i -> cs.[len - 1 - i])