#use "./../assign1.ml";;

#use "./../MyOCaml.ml";;

(* Function to check if a string is ascending *)
  let is_ascending s =
    let rec aux i =
      if i >= string_length s - 1 then true
      else if string_get_at s i >= string_get_at s (i + 1) then false
      else aux (i + 1)
    in
    aux 0
  
  (* Function to create a string of length n with a repeated character c *)
  let string_make_char n c =
    let rec aux acc i =
      if i <= 0 then acc else aux (string_cons c acc) (i - 1)
    in
    aux "" n
  
  (* Function to find the longest ascending subsequence of a string *)
  let string_longest_ascend xs =
    let rec find_max_subseq start max_seq cur_seq =
      if start >= string_length xs then
        if string_length cur_seq > string_length max_seq then cur_seq else max_seq
      else if is_ascending cur_seq then
        find_max_subseq (start + 1) max_seq (string_cons (string_get_at xs start) cur_seq)
      else
        find_max_subseq (start + 1) (if string_length cur_seq > string_length max_seq then cur_seq else max_seq) (string_make_char 1 (string_get_at xs start))
    in
    find_max_subseq 0 "" ""
;;
