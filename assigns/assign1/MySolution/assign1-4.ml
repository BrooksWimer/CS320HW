#use "./../assign1.ml";;

#use "./../MyOCaml.ml";;

(*
  Add two integer representations stored as strings.
  Parameters:
    - ds1: The first integer as a string.
    - ds2: The second integer as a string.
  Returns:
    - The sum of ds1 and ds2 as a string.
*)
let intrep_add(ds1: string)(ds2: string): string =
  (* Get the lengths of the input strings ds1 and ds2. *)
  let len1 = string_length ds1 in
  let len2 = string_length ds2 in

  (* Define a recursive helper function for addition. *)
  let rec intrep_add_helper (i: int) (j: int) (carry: int) (work: string): string =
    (* If both indices are less than 0 and there is no carry, return the result. *)
    if i < 0 && j < 0 && carry = 0 then
      work
    else
      (* Get the characters at positions i and j in ds1 and ds2. *)
      let char1 = if i >= 0 then string_get_at ds1 i else '0' in
      let char2 = if j >= 0 then string_get_at ds2 j else '0' in

      (* Convert characters to integers and calculate the sum with carry. *)
      let num1 = ord char1 - 48 in
      let num2 = ord char2 - 48 in

      (* Check if the sum requires a carry. *)
      if num1 + num2 + carry > 9 then
        let new_digit = (num1 + num2 + carry) mod 10 in
        (* Recursively call the function with updated values. *)
        intrep_add_helper (i - 1) (j - 1) 1 (string_cons (char_of_digit new_digit) work)
      else
        let new_digit = num1 + num2 + carry in
        (* Recursively call the function with updated values. *)
        intrep_add_helper (i - 1) (j - 1) 0 (string_cons (char_of_digit new_digit) work)
  in

  (* Start the helper function with initial values. *)
  intrep_add_helper (len1 - 1) (len2 - 1) 0 ""
;;
    