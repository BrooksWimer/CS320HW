(* ****** ****** *)

#use "./../assign0.ml";;

#use "./../MyOCaml.ml";;

(* ****** ****** *)

let int2str i0 =
  let res = ref "" in
  let rec helper n =
    if n < 0 then begin
      let x = char_of_digit(n) in
      res := string_cons(x)(!res);
      helper (-n)
    end
    else if n < 10 then
      res := string_cons(char_of_digit(n))(!res)
    else begin
      res := string_cons(char_of_digit(n mod 10))(!res);
      helper (n / 10);
    end
  in
  helper i0;
  !res

;;