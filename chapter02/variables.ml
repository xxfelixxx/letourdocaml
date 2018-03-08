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

let join_comma ints =
  String.concat ~sep:","
       (List.map ~f:(fun x -> Int.to_string x) ints )

(* anonymous functions are introduced with keyword 'fun'
                           .--- anon ---.                 *)
let squares = List.map ~f:(fun x -> x * x) [1;2;3;4]

(* store 2 anonymous functions *)
let increments = [ (fun x -> x + 1); (fun x -> x + 2) ]
(* invoke them through another anonymous function *)
let successors_of_3 = List.map ~f:(fun g -> g 3) increments

(* functions can be declared in 2 equivalent ways:
    let plusone = (fun x -> x + 1)
        -- or --
    let plusone x = x + 1
 *)

(* multiarg functions *)
let abs_diff x y = abs (x - y)

(* equivalently:
    let abs_diff = ( fun x -> ( fun y -> abs ( x - y ) ) )

    i.e. abs_diff is a function of x which returns a function of y
         which returns the absolute difference of x and y.

    abs_diff here is a curried function (named after Haskell Curry) and
    its type signature is:

        val abs_diff : int -> int -> int

     -- equivalently through right associativity: --

        val abs_diff : int -> ( int -> int )
 *)


let () =
  printf "--- variables.ml ---\n";
  printf "Replacing , with - : %s -> %s\n" languages dashed_languages;
  printf "Area of ring with inner radius 3 and outer radius 4 is %F\n"
    (area_of_ring 3. 4.);
  printf "Uppercasing first element of list : %s\n"
    (upcase_first_entry "foo,bar,baz");
  printf "First 4 squares are : %s\n" (join_comma squares);
  printf "The successors of 3 are : %s\n" (join_comma successors_of_3);
  printf "The abs difference between -3 and 4 : %d\n" ( abs_diff (-3) 4 );
