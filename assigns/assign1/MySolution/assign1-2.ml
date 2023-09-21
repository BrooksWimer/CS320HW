(* ****** ****** *)

#use "./../assign1.ml";;

#use "./../MyOCaml.ml";;

(* ****** ****** *)

let string_merge(cs1) (cs2)=
    let l1 = string_length cs1 in
    let l2 = string_length cs1 in
    let rec aux index1 index2 work =
        if (index1 = l1) then
          int1_foreach(l2-index2) (fun newIndex-> work(string_get_at(cs2)(index2 + newIndex)))
        else if (index2 = l2) then 
          int1_foreach(l1-index1) (fun newIndex -> work(string_get_at(cs1)(index1+newIndex)))
        
        else
          let char1 = string_get_at(cs1)(index1) in
          let char2 = string_get_at(cs2)(index2) in

          if char1 < char2 then
            (work(char1); aux(index1+1)(index2)(work))
          else 
            (work(char2); aux(index1)(index2+1)(work))
    in
    string_make_fwork(aux(0)(0))

;;
    


  

