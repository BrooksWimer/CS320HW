(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let stringrev(cs: string): string =
  let len = String.length cs in
  String.init len (fun i -> cs.[len - 1 - i])