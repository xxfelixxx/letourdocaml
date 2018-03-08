(* sum of numbers typed into stdin          
   first example program from rwo chapter 1 *)
open Core
   
let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate ( accum +. Float.of_string x )

let () =
  printf "Total: %F\n" (read_and_accumulate 0.)
