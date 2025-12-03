open Angstrom
open Base

let test_input = {|
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
|}

let whitespace =
  skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let number = take_while1 (function '0' .. '9' -> true | _ -> false)
let left = char 'L' *> number >>= fun s -> return (`L (Int.of_string s))
let right = char 'R' *> number >>= fun s -> return (`R (Int.of_string s))
let line = left <|> right
let lines = option () whitespace *> many (line <* char '\n')
let unwrap = function Ok x -> x | Error msg -> failwith msg
let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let swap f x y = f y x
let parse_input = unwrap << parse_string ~consume:All lines
let read_file = (swap In_channel.with_open_text) In_channel.input_all

let scan ~init ~f xs =
  let init = (init, []) in
  let _, ys =
    List.fold ~init
      ~f:(fun (acc, ys) x ->
        let new_value = f acc x in
        (new_value, new_value :: ys))
      xs
  in
  List.rev ys

let part_1 input =
  input
  |> scan ~init:50 ~f:(fun acc -> function
       | `R n -> (acc + n) % 100 | `L n -> (acc - n) % 100)
  |> List.filter ~f:(function 0 -> true | _ -> false)
  |> List.length

let part_2 input =
  input
  |> List.map ~f:(function `R n -> n | `L n -> -1 * n)
  |> List.fold ~init:(0, 50) ~f:(fun (count, pos) movement ->
         let new_pos = pos + movement in
         let offset =
           Int.abs (new_pos / 100) + if new_pos <= 0 && pos <> 0 then 1 else 0
         in
         (count + offset, new_pos % 100))
  |> fst

let test_input_1 = {|
L1000
|}

let main () =
  let input = read_file "input/day_1.txt" in
  let res = part_1 @@ parse_input input in
  Stdio.printf "Part 1 %d\n" res;
  let res = part_2 @@ parse_input input in
  Stdio.printf "Part 2 %d\n" res
