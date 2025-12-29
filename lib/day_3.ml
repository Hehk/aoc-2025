open Angstrom
open Base

let unwrap = function Ok x -> x | Error msg -> failwith msg
let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let swap f x y = f y x

let test_input =
  {|
987654321111111
811111111111119
234234234234278
818181911112111
|}

let whitespace =
  take_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let digit =
  satisfy (function '0' .. '9' -> true | _ -> false)
  >>= (return << Int.of_string << String.of_char)

let line = many digit
let lines = sep_by (char '\n') line
let parse_input = unwrap << parse_string ~consume:All lines << String.strip
let read_file = (swap In_channel.with_open_text) In_channel.input_all

let find_index n =
  List.foldi ~init:None ~f:(fun i acc x ->
      match (acc, x = n) with
      | Some x, _ -> Some x
      | _, true -> Some i
      | _, _ -> None)

let max_n xs =
  xs
  |> List.max_elt ~compare:( - )
  |> Option.map ~f:(fun n -> (n, Option.value_exn @@ find_index n xs))

let get_code xs =
  let n, i =
    Option.value_exn @@ max_n @@ List.sub ~pos:0 ~len:(List.length xs - 1) xs
  in
  let m, _ =
    Option.value_exn @@ max_n
    @@ List.sub ~pos:(i + 1) ~len:(List.length xs - i - 1) xs
  in
  (n * 10) + m

let get_code_2 size xs =
  let rec aux start acc =
    match List.length acc with
    | n when n >= size -> acc
    | n ->
        let len = List.length xs - (size - n) - (start - 1) in
        let xs = List.sub ~pos:start ~len xs in
        let n, i = Option.value_exn @@ max_n xs in
        aux (i + 1 + start) (n :: acc)
  in
  aux 0 []
  |> List.mapi ~f:(fun i x -> Int.pow 10 i * x)
  |> List.fold ~init:0 ~f:( + )

let main () =
  let input = parse_input @@ read_file "input/day_3.txt" in
  let res = input |> List.map ~f:get_code |> List.fold ~init:0 ~f:( + ) in
  Stdio.printf "Part 1 %d\n" res;
  let input = parse_input @@ read_file "input/day_3.txt" in
  let res =
    input |> List.map ~f:(get_code_2 12) |> List.fold ~init:0 ~f:( + )
  in
  Stdio.printf "Part 2 %d\n" res;
  ()
