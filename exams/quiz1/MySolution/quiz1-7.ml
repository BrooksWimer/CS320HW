#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

let isPrime(n) =
  let test(i:int): bool = (* YOUR CODE *)
  let div = 2 in 
  let rec aux div = 
    if div > int_sqrt(n) then
      true
    else if n mod div = 0 then 
      false
    else aux (div+1)
  in
  aux 2
in

  if n < 2 then false else int1_forall(n)(test)
;;

(* ************************************************ *)
