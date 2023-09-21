#use "./../assign1.ml";;

#use "./../MyOCaml.ml";;

(*
  Function to check if a specific condition is met.
  Parameters:
    - cs: The input string.
    - a: Index of the first character.
    - b: Index of the second character.
    - c: Index of the third character.
  Returns:
    - true if the condition is satisfied, false otherwise.
*)
    let has_condition(cs: string)(a)(b)(c): bool =
    let va = string_get_at cs a in
    let vb = string_get_at cs b in
    let vc = string_get_at cs c in
    if va < vc && vc < vb then
      true
    else
      false
  ;;
  
  (*
    Function to check if a string avoids the specific 132 pattern.
    Parameters:
      - cs: The input string to be checked.
    Returns:
      - true if cs avoids the 132 pattern, false otherwise.
  *)
  let string_avoid_132(cs: string): bool =
    if string_length cs < 3 then
      true  (* A string with fewer than 3 characters automatically avoids the pattern. *)
    else if string_length cs = 3 then
      if has_condition cs 0 1 2 then
        false
      else
        true
    else
      let len = string_length cs in
      let rec check_avoidance a b c =
        (* Check if the current substring satisfies the 132 pattern. *)
        if has_condition cs a b c then
          false
        else
          if a = len - 3 then
            true
          else if b <> len - 2 then
            (* Move to the next substring by incrementing indices. *)
            check_avoidance (a + 1) (a + 2) (a + 3)
          else
            (* Move to the next substring by incrementing indices. *)
            check_avoidance a b (c + 1)
      in
      (* Start the helper function with initial indices. *)
      check_avoidance 0 1 2
  ;;
     


