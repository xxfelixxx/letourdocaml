open Core

let one_two_three = [1;2;3]
let one_two_three_v2 = 1 :: (2 :: (3 :: []))
let one_two_three_v3 = 1 :: 2 :: 3 :: []
                     
let join_int_list list =
  String.concat ~sep:","
    ( List.map ~f:(fun x -> Int.to_string x) list )
  
let () =
  printf "semicolons can be used to defined lists\n";
  printf "[1;2;3]-> %s\n" ( join_int_list one_two_three );
  printf "1 :: (2 :: (3 :: [])) -> %s\n" ( join_int_list one_two_three_v2 );
  printf ":: is right associative, so parens are not necessary\n";
  printf "1 :: 2 :: 3 :: [] -> %s\n" ( join_int_list one_two_three_v3 );  
  printf "empty list [] -> '%s'\n" ( join_int_list [] );
  printf "OCaml lists are stored internally as singly-linked lists\n";
  printf "    1 -> 2 -> 3 -> null\n";
  
    
    
