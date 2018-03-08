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

let distance_from_3 = abs_diff 3

let abs_diff_curried_style = ( fun x y -> abs (x - y ) )
let abs_diff_tuple_style (x,y) = abs (x - y)

let rec fib n =
  match n with
  | 0 -> 1
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

let rec find_first_stutter list =
  match list with
  | [] | [_] -> (* only zero or one elements, so no repeats *)
     None
  | x :: y :: tl ->
     if x = y then Some x else find_first_stutter (y::tl)

(* mutually recursive functions *)
let rec is_even x =
  if x = 0 then true else is_odd (x - 1)
and is_odd x =
  if x = 0 then false else is_even (x - 1)

(* new fancy 2-tuple adder *)
let (+!) (x1,y1) (x2,y2) = ( x1 + x2, y1 + y2 )
let join_comma_duple (x,y) =
  String.concat ~sep:"," (List.map ~f:(fun x -> Int.to_string x) [x;y])

(* new fancy power-up operator...note that spaces are required around *** *)
let ( *** ) x y = ( x ** y ) ** y

(* pipe operator of a value and a function is the result of applying
   the function to that value *)
let (|>) x f = f x

(* declaring functions with keyword 'function' have automatic matching *)
let some_or_zero = function
  | Some x -> x
  | None -> 0

let some_or_default default = function
  | Some x -> x
  | None -> default

(* test everything and print it all out *)
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
  printf "The abs difference from 3 of 4 : %d\n" ( distance_from_3 4 );
  printf "The abs difference between -3 and 4 curried style: %d\n"
    ( abs_diff_curried_style (-3) 4 );
  printf "The abs difference between -3 and 4 tuple style: %d\n"
    ( abs_diff_tuple_style (-3, 4) );
  printf "The value of fib(10) is %d\n" (fib 10);
  let stut =
    let stutter = find_first_stutter [1;2;3;3;4] in
    match stutter with
      | None -> "nothing to see here"
      | Some x -> Int.to_string x
  in
  printf "The first stutter of 1,2,3,3,4 is %s\n" (stut);

  printf "Is 7 even ? %s.\n" (Bool.to_string (is_even 7));
  printf "Is 7 odd  ? %s.\n" (Bool.to_string (is_odd 7 ));
  printf "Is 8 even ? %s.\n" (Bool.to_string (is_even 8));
  printf "Is 8 odd  ? %s.\n" (Bool.to_string (is_odd 8 ));
  let res = (+!) (1,2) (3,4) in
  printf "The vector sum of (1,2) and (3,4) is (%s)\n"
    (join_comma_duple res);
  printf "The powerup of 2 and 3 is %F\n" (( *** ) 2. 3. );

  printf "Here are some paths:\n";
  let path = "/usr/bin:/usr/local/bin:/bin/sbin" in
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:(fun x -> printf "--> %s\n" x);

  printf "Some_or_zero of ( Some 4 ) is %d\n" (some_or_zero (Some 4));
  printf "Some_or_zero of ( None   ) is %d\n" (some_or_zero None);

  printf "Some_or_default 100 of [ Some 3; None; Some 4 ] is %s\n"
    ( join_comma (List.map ~f:(some_or_default 100) [Some 3; None; Some 4]) )
