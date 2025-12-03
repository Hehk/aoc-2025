module Day_1 = Day_1
module Day_2 = Day_2

(* CLAUDE WROTE THIS DOOZEY BUT IT WORKS... *)

let run_day day =
  match day with
  | 1 -> Day_1.main ()
  | 2 -> Day_2.main ()
  | _ ->
      Printf.eprintf "Error: Day %d not implemented yet\n" day;
      exit 1

let parse_day_string s = try Some (int_of_string s) with Failure _ -> None

let main () =
  let usage_msg = "aoc <day_number>" in
  let day_ref = ref None in

  let set_day s =
    match parse_day_string s with
    | Some d when d > 0 -> day_ref := Some d
    | _ ->
        Printf.eprintf
          "Error: Invalid day number '%s'. Must be a positive integer.\n" s;
        exit 1
  in

  let spec_list = [] in

  Arg.parse spec_list set_day usage_msg;

  match !day_ref with
  | Some day -> run_day day
  | None ->
      Printf.eprintf "Usage: %s\n" usage_msg;
      exit 1

