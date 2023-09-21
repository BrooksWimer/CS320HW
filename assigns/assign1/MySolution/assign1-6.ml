#use "./../assign1.ml";;

#use "./../MyOCaml.ml";;

(*
  Check if the condition (cs[a] < cs[c]) && (cs[c] < cs[b]) && (cs[b] < cs[d]) is true.
  Parameters:
    - cs: The input string.
    - a: The index of the first character.
    - b: The index of the second character.
    - c: The index of the third character.
    - d: The index of the fourth character.
  Returns:
    - true if the condition is satisfied, false otherwise.
*)
    let is1324(cs: string)(a)(b)(c)(d): bool =
    if string_get_at cs a < string_get_at cs c &&
       string_get_at cs c < string_get_at cs b &&
       string_get_at cs b < string_get_at cs d then
      true
    else
      false
  ;;
  
  (*
    Check if the input string cs avoids the 1324 pattern.
    Parameters:
      - cs: The input string to be checked.
    Returns:
      - true if cs avoids the 1324 pattern, false otherwise.
  *)
  let string_avoid_1324(cs: string): bool =
    let len = string_length cs in
    if len < 4 then
      true
    else
      let rec string_avoid_1324_helper(i: int)(j: int)(k: int)(l: int): bool =
        (* Check if the current substring satisfies the 1324 pattern. *)
        if is1324 cs i j k l then
          false
        else if l <> len - 1 then
          (* Move to the next substring by incrementing indices. *)
          string_avoid_1324_helper i (j + 1) (k + 1) (l + 1)
        else if i = len - 4 then
          (* If we've checked all substrings and none satisfy the pattern, return true. *)
          true
        else
          (* Move to the next substring by incrementing indices. *)
          string_avoid_1324_helper (i + 1) (i + 2) (i + 3) (i + 4)
      in
      (* Start the helper function with initial indices. *)
      string_avoid_1324_helper 0 1 2 3
  ;;





