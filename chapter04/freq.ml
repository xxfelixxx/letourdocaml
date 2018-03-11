open Core

(* use touch function from counter.ml *)
let build_counts () =
  In_channel.fold_lines In_channel.stdin ~init:[] ~f:Counter.touch

  (* Original implementation:
   *
   * https://stackoverflow.com/questions/45029549/example-code-from-real-world-ocaml-doesnt-run-as-intended
   *
   * let build_counts () =
   *   In_channel.fold_lines In_channel.stdin ~init:[] ~f:(fun counts line ->
   *       let count =
   *         match List.Assoc.find counts ~equal:(=) line with
   *         | None -> 0
   *         | Some x -> x
   *       in
   *       List.Assoc.add counts ~equal:(=) line (count + 1)
   *     ) *)

(* let () means pattern match that the right-hand side returns 'unit'
   which is typical for functions with side-effects, i.e. printing *)  
let () =
  build_counts ()
  |> List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  |> (fun l -> List.take l 10)
  |> List.iter ~f:(fun (line,count) -> printf "%3d: %s\n" count line)
