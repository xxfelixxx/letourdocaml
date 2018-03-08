open Core
   
let languages = "OCaml,Perl,C++,C"
let dashed_languages =
  let language_list = String.split languages ~on:',' in
  String.concat ~sep:"-" language_list

let area_of_ring inner_radius outer_radius =
  let pi = Float.acos (-1.) in
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius

(* tuples are separated by commas      ','
    lists are separated by semi-colons ';'
    (ints,strings) is a pattern            *)
let (ints,strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")]

let upcase_first_entry line =
  match String.split ~on:',' line with
  | [] -> assert false (* String.split returns at least one element *)
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)

let () =
  printf "--- variables.ml ---\n";
  printf "Replacing , with - : %s -> %s\n" languages dashed_languages;
  printf "Area of ring with inner radius 3 and outer radius 4 is %F\n"
    (area_of_ring 3. 4.);
  printf "Uppercasing first element of list : %s\n"
    (upcase_first_entry "foo,bar,baz");
