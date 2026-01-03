open Angstrom
open Base

let test_input =
  {|
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
|}

type point = Paper | Empty

let whitespace =
  skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let line = many1 (char '.' *> return Empty <|> char '@' *> return Paper)
let lines = sep_by (char '\n') line
let unwrap = function Ok x -> x | Error msg -> failwith msg
let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let parse_input = unwrap << parse_string ~consume:All lines << String.strip
let swap f x y = f y x
let read_file = (swap In_channel.with_open_text) In_channel.input_all

let iter f lines =
  List.iteri ~f:(fun i line -> List.iteri ~f:(fun j x -> f i j x) line) lines

let get_neighbors i j lines =
  let get i j =
    match List.nth lines i with None -> None | Some row -> List.nth row j
  in
  let top_left = get (i - 1) (j - 1) in
  let top = get (i - 1) j in
  let top_right = get (i - 1) (j + 1) in
  let left = get i (j - 1) in
  let right = get i (j + 1) in
  let bottom_left = get (i + 1) (j - 1) in
  let bottom = get (i + 1) j in
  let bottom_right = get (i + 1) (j + 1) in
  [ top_left; top; top_right; left; right; bottom_left; bottom; bottom_right ]

let can_access neighbors =
  neighbors
  |> List.filter ~f:(function Some Paper -> true | _ -> false)
  |> List.length < 4

let is_paper = function Paper -> true | _ -> false

let part_1 lines =
  let count = ref 0 in
  lines
  |> iter (fun i j x ->
         if is_paper x && lines |> get_neighbors i j |> can_access then
           count := !count + 1
         else ());
  count

let map f lines =
  List.mapi ~f:(fun i line -> List.mapi ~f:(fun j x -> f i j x) line) lines

let part_2 lines =
  let count = ref 0 in
  let rec aux prev_count lines =
    let lines =
      lines
      |> map (fun i j x ->
             match x with
             | Paper when lines |> get_neighbors i j |> can_access ->
                 count := !count + 1;
                 Empty
             | Paper -> Paper
             | _ -> Empty)
    in
    if prev_count = !count then count else aux !count lines
  in
  aux 0 lines
