#use "./../assign1.ml";;

#use "./../MyOCaml.ml";;

(*
  Merge two strings while preserving the order of characters.
  Parameters:
    - cs1: The first input string.
    - cs2: The second input string.
  Returns:
    - The merged string with characters in non-decreasing order.
*)
let merge_strings(cs1)(cs2) =
  let l1 = string_length cs1 in
  let l2 = string_length cs2 in

  (* Define a recursive auxiliary function for merging. *)
  let rec merge_helper index1 index2 work =
    if index1 = l1 then
      (* If cs1 is fully processed, append remaining characters from cs2. *)
      int1_foreach (l2 - index2) (fun newIndex -> work (string_get_at cs2 (index2 + newIndex)))
    else if index2 = l2 then
      (* If cs2 is fully processed, append remaining characters from cs1. *)
      int1_foreach (l1 - index1) (fun newIndex -> work (string_get_at cs1 (index1 + newIndex)))
    else
      let char1 = string_get_at cs1 index1 in
      let char2 = string_get_at cs2 index2 in

      if char1 < char2 then
        (* Append the smaller character from cs1 and move to the next index. *)
        (work char1; merge_helper (index1 + 1) index2 work)
      else
        (* Append the smaller character from cs2 and move to the next index. *)
        (work char2; merge_helper index1 (index2 + 1) work)
  in
  (* Start the merging process with initial indices. *)
  string_make_fwork (merge_helper 0 0)
;;