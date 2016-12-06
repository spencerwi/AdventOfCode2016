module Keypad = struct
    type direction = U | D | L | R
    let move (current_key: int) = function
        | U when List.mem current_key [1;2;3] -> current_key
        | U -> current_key - 3
        | D when List.mem current_key [7;8;9] -> current_key
        | D -> current_key + 3
        | L when List.mem current_key [1;4;7] -> current_key
        | L -> current_key - 1
        | R when List.mem current_key [3;6;9] -> current_key
        | R -> current_key + 1

    let parse_direction = function
        | "U" -> U
        | "D" -> D
        | "L" -> L
        | "R" -> R
        | other -> failwith ("Bad direction: " ^ other) 
end

let read_day2() : Keypad.direction list list =
    AdventStdLib.read_lines_from_file "../inputs/day2.txt"
    |> List.map AdventStdLib.explode_str
    |> List.map (List.map Keypad.parse_direction)

let day2a() : int list =
    read_day2()
    |> List.map (List.fold_left Keypad.move 5) 

let () =
    let day2a_code = day2a() |> List.map string_of_int |> String.concat "" in
    Printf.printf "Day 2A: %s" day2a_code

