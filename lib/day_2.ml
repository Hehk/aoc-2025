open Angstrom
open Base

let test_input =
  {|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124|}

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let swap f x y = f y x
let number = take_while1 (function '0' .. '9' -> true | _ -> false)

let range =
  let make_tuple x y = (x, y) in
  make_tuple
  <$> (number >>= (return << Int.of_string))
  <* char '-'
  <*> (number >>= (return << Int.of_string))

let ranges = sep_by (char ',') range
let unwrap = function Ok x -> x | Error msg -> failwith msg
let parse_input = unwrap << parse_string ~consume:All ranges << String.strip
let read_file = (swap In_channel.with_open_text) In_channel.input_all

let tuple_to_range tuple =
  let rec aux n acc = if n < fst tuple then acc else aux (n - 1) (n :: acc) in
  aux (snd tuple) []

let chunks size str =
  let len = String.length str in
  let rec aux = function
    | n when n >= len -> []
    | n ->
        let size = if n + size >= len then len - n else size in
        let chunk = String.sub ~pos:n ~len:size str in
        chunk :: aux (n + size)
  in
  aux 0

let all_chunks_equal chunks =
  match List.hd chunks with
  | None -> true
  | Some hd -> List.for_all ~f:(String.equal hd) chunks

let is_valid id =
  let id_s = Int.to_string id in
  let half = String.length id_s / 2 in
  let first_half = String.sub id_s ~pos:0 ~len:half in
  let second_half =
    String.sub id_s ~pos:half ~len:(String.length id_s - half)
  in
  not @@ String.equal first_half second_half

let is_invalid_part_2 id =
  let id_s = Int.to_string id in
  let rec is_invalid = function
    | n when n > String.length id_s / 2 -> false
    | n -> if all_chunks_equal @@ chunks n id_s then true else is_invalid (n + 1)
  in
  is_invalid 1

let part_1 i =
  i |> List.map ~f:tuple_to_range |> List.join
  |> List.filter ~f:(not << is_valid)
  |> List.fold ~init:0 ~f:( + )

let part_2 i =
  i |> List.map ~f:tuple_to_range |> List.join
  |> List.filter ~f:is_invalid_part_2
  |> List.fold ~init:0 ~f:( + )

let main () =
  let input = read_file "input/day_2.txt" in
  let res = part_1 @@ parse_input input in
  Stdio.printf "Part 1 %d\n" res;
  let res = part_2 @@ parse_input test_input in
  Stdio.printf "Part 2 %d\n" res
