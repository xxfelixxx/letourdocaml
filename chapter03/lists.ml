open Core
open Core_bench.Std

let one_two_three_v1 = [1;2;3]
let one_two_three_v2 = 1 :: (2 :: (3 :: []))
let one_two_three_v3 = 1 :: 2 :: 3 :: []
                     
let join_int_list list =
  String.concat ~sep:","
    ( List.map ~f:(fun x -> Int.to_string x) list )

(* match reads data out from a list *)
let rec sum l =
  match l with
  | [] -> 0
  | hd :: tl -> hd + sum tl

let rec drop_value l to_drop =
  match l with
  | [] -> []
  | hd :: tl ->
     let new_tl = drop_value tl to_drop in
     if hd = to_drop then new_tl else hd :: new_tl

let rec drop_zero l =
  match l with
  | [] -> []
  | 0 :: tl -> drop_zero tl
  | hd :: tl -> hd :: drop_zero tl

let rec drop_zero_v2 l = drop_value l 0

(* performance of pattern matching vs if/else chains *)
let plus_one_match x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | _ -> x + 1

let plus_one_if x =
  if      x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else x + 1

let rec sum_if l =
  if List.is_empty l then 0
  else List.hd_exn l + sum_if (List.tl_exn l)

(* create a table printing function *)
let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

let render_separator widths =
  let pieces =
    List.map widths
      ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"

let pad s length =
  " " ^ s ^ String.make ( length - String.length s + 1 ) ' '

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (   render_row header widths
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row row widths)
    )

(* more useful List.* functions *)
let reduced_sum = List.reduce ~f:(+) [1;2;3;4;5]

let is_ocaml_source s =
  match String.rsplit2 s ~on:'.' with
  | Some (_,("ml"|"mli")) -> true
  | _ -> false

let (ml_files, other_files) =
  List.partition_tf (Sys.ls_dir ".") ~f:is_ocaml_source

(* ^/ is an infix operator for adding a new element to a string *)
let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.map ~f:(fun sub -> ls_rec ( s ^/ sub))
    |> List.concat

let rec ls_rec2 s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.concat_map ~f:(fun sub -> ls_rec (s ^/ sub))

(* tail recursion *)

let make_list n = List.init n ~f:(fun x -> x)

(* this causes a stack overflow on large list sizes! *)
(* this will happily allocate 1_000_000 stack frames... *)
let rec length = function
  | [] -> 0
  | _ :: tl -> 1 + length tl

let rec length_plus_n l n =
  match l with
  | [] -> n
  | _ :: tl -> length_plus_n tl (n + 1)

(* tail call optimization re-uses the same stack frame *)
let length2 l = length_plus_n l 0

let rec destutter list =
  match list with
  | [] -> []
  | [hd] -> [hd]
  | hd :: hd' :: tl ->
     if hd = hd' then destutter (hd' :: tl)
     else hd :: destutter (hd' :: tl)

(* improve destutter by removing match and eliminating allocation of [hd] *)
let rec destutter2 = function
  | []  as l -> l
  | [_] as l -> l
  | hd :: (hd' :: _ as tl) ->
     if hd = hd' then destutter2 tl
     else hd :: destutter2 tl

(* Combine first two cases using an Or-pattern *)
let rec destutter3 = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) ->
     if hd = hd' then destutter3 tl
     else hd :: destutter3 tl

(* Add a when clause to simplify slightly *)
let rec destutter4 = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) when hd = hd' -> destutter4 tl
  | hd :: tl -> hd :: destutter4 tl

(*
 * count_some will throw a warning:
 *     this pattern-matching is not exhaustive.
 *     Here is an example of a case that is not matched:
 *     _::_
 *     (However, some guarded clause may match this value.)
 *)

let rec count_some list =
  match list with
  | [] -> 0
  | x :: tl when Option.is_none x -> count_some tl
  | x :: tl when Option.is_some x -> 1 + count_some tl

(* remove warning with a redundent match case *)
let rec count_some2 list =
  match list with
  | [] -> 0
  | x :: tl when Option.is_none x -> count_some2 tl
  | x :: tl when Option.is_some x -> 1 + count_some2 tl
  | x :: tl -> -1 (* unreachable *)

(* better approach *)
let rec count_some3 list =
  match list with
  | [] -> 0
  | x :: tl when Option.is_none x -> count_some3 tl
  | _ :: tl -> 1 + count_some3 tl

(* a clearer approach *)
let rec count_some4 list =
  match list with
  | [] -> 0
  | None :: tl -> count_some4 tl
  | Some _ :: tl -> 1 + count_some4 tl

(* better just to use List.count for this! *)
let rec count_some5 list = List.count ~f:Option.is_some list

let () =
  printf "semicolons can be used to defined lists\n";
  printf "[1;2;3]-> %s\n" ( join_int_list one_two_three_v1 );
  printf "1 :: (2 :: (3 :: [])) -> %s\n" ( join_int_list one_two_three_v2 );
  printf ":: is right associative, so parens are not necessary\n";
  printf "1 :: 2 :: 3 :: [] -> %s\n" ( join_int_list one_two_three_v3 );  
  printf "empty list [] -> '%s'\n" ( join_int_list [] );
  printf "OCaml lists are stored internally as singly-linked lists\n";
  printf "    1 -> 2 -> 3 -> null\n";
  printf "sum [1;2;3] -> %d\n" (sum one_two_three_v1);
  printf "sum [] -> %d\n" (sum []);
  printf "drop_value 3 [1;2;3] -> %s\n"
    ( join_int_list ( drop_value [1;2;3] 3 ));
  printf "drop_value 3 [1;2;3;1;2;3] -> %s\n"
    ( join_int_list ( drop_value [1;2;3;1;2;3] 3 ));
  printf "drop_zero    [0;1;2;3;0;1;2;3] -> %s\n"
    ( join_int_list ( drop_zero [0;1;2;3;0;1;2;3] ));
  printf "drop_zero_v2 [0;1;2;3;0;1;2;3] -> %s\n"
    ( join_int_list ( drop_zero_v2 [0;1;2;3;0;1;2;3] ));
  printf "plus_one_match 5 -> %d\n" ( plus_one_match 5 );
  printf "plus_one_if 5    -> %d\n" ( plus_one_if 5 );

  printf "render_separator [3;6;2] : %s\n" (render_separator [3;6;2]);
  printf "pad \"foo\" 10 : '%s'\n" (pad "foo" 10);
  printf "render_row [\"Hello\";\"World\"] [10;15] : '%s'\n"
    ( render_row [ "Hello";"World" ] [ 10;15 ] );
  printf "table:\n%s\n"
    ( render_table
        [ "Name"; "Title" ] [
          [ "Hello"; "World" ];
          [ "Goodbye"; "World" ]
        ] );

  let total x =
    match x with
    | Some x -> x
    | None -> 0 in
  printf "reduced_sum : %d\n" (total reduced_sum);

  let evens x =
    List.filter ~f:(fun x -> x mod 2 = 0) x in
  printf "evens: %s\n" (join_int_list (evens [0;1;2;3;4;5;6] ) );

  let exts = List.filter_map (Sys.ls_dir ".")
               ~f:(fun fname -> match String.rsplit2 ~on:'.' fname with
                                | None | Some ("",_) -> None
                                | Some (_,ext) -> Some ext
               )
             |> List.dedup_and_sort in
  printf "Some file extensions: %s\n" (String.concat ~sep:"|" exts);

  printf "Some ml files: '%s'\n" (String.concat ~sep:"," ml_files);
  printf "Some other files: '%s'\n" (String.concat ~sep:"," other_files);

  printf "Combining lists:\n";
  printf "List.append [1;2;3] [4;5;6] -> %s\n"
    ( join_int_list ( List.append [1;2;3] [4;5;6] ));

  printf "[1;2;3] @ [4;5;6] -> %s\n"
    ( join_int_list ( [1;2;3] @ [4;5;6] ));

  printf "List.concat [[1;2];[3;4;5];[6;7];[]] -> %s\n"
    ( join_int_list ( List.concat [[1;2];[3;4;5];[6;7];[]] ));

  let dir = "/usr/local/lib/ocaml/caml" in
  (
    printf "Looking in %s\n" dir;
    printf "ls_rec  ---> \n%s\n" ( String.concat ~sep:"\n" (ls_rec  dir) );
    printf "ls_rec2 ---> \n%s\n" ( String.concat ~sep:"\n" (ls_rec2 dir) );
  );

  printf "Length of [1;2;3] is %d\n" ( length [1;2;3] );

  printf "Length of [1;...;1_000_000] is %d\n"
    ( length2 (make_list 1_000_000) );

  printf "destutter  [1;2;3;4;4;3;2;1] is %s\n"
    ( join_int_list ( destutter [1;2;3;4;4;3;2;1] ) );
  printf "destutter2 [1;2;3;4;4;3;2;1] is %s\n"
    ( join_int_list ( destutter2 [1;2;3;4;4;3;2;1] ) );
  printf "destutter3 [1;2;3;4;4;3;2;1] is %s\n"
    ( join_int_list ( destutter3 [1;2;3;4;4;3;2;1] ) );
  printf "destutter4 [1;2;3;4;4;3;2;1] is %s\n"
    ( join_int_list ( destutter4 [1;2;3;4;4;3;2;1] ) );

  printf "count_some  [ None; Some 3; Some 4 ] -> %d\n"
    ( count_some  [ None; Some 3; Some 4 ] );
  printf "count_some2 [ None; Some 3; Some 4 ] -> %d\n"
    ( count_some2 [ None; Some 3; Some 4 ] );
  printf "count_some3 [ None; Some 3; Some 4 ] -> %d\n"
    ( count_some3 [ None; Some 3; Some 4 ] );
  printf "count_some4 [ None; Some 3; Some 4 ] -> %d\n"
    ( count_some4 [ None; Some 3; Some 4 ] );
  printf "count_some5 [ None; Some 3; Some 4 ] -> %d\n"
    ( count_some5 [ None; Some 3; Some 4 ] );

  (* plus_one_match should be much faster than plus_one_if *)
  [
    Bench.Test.create ~name:"plus_one_match" (fun () -> ignore (plus_one_match 10));
    Bench.Test.create ~name:"plus_one_if"    (fun () -> ignore (plus_one_if    10))
  ] |> Bench.bench;

  (* sum should be much faster than sum_if *)
  let numbers = List.range 0 1000 in
  [
    Bench.Test.create ~name:"sum_if" (fun () -> ignore (sum_if numbers));
    Bench.Test.create ~name:"sum"    (fun () -> ignore (sum   numbers));
  ] |> Bench.bench;

  
