module Keypad = struct
    type direction = U | D | L | R
    let parse_direction = function
        | "U" -> U
        | "D" -> D
        | "L" -> L
        | "R" -> R
        | other -> failwith ("Bad direction: " ^ other) 

    let move_on_simple_keypad (current_key: int) = function
        | U when List.mem current_key [1;2;3] -> current_key
        | U -> current_key - 3
        | D when List.mem current_key [7;8;9] -> current_key
        | D -> current_key + 3
        | L when List.mem current_key [1;4;7] -> current_key
        | L -> current_key - 1
        | R when List.mem current_key [3;6;9] -> current_key
        | R -> current_key + 1

    let move_on_complex_keypad (current_key: string) move =
        match move with 
        | U when List.mem current_key ["1";"2";"5";"4";"9"] -> current_key
        | U -> begin
            match current_key with
                         | "3" -> "1"
            | "6" -> "2" | "7" -> "3" | "8" -> "4"
            | "A" -> "6" | "B" -> "7" | "C" -> "8"
                         | "D" -> "B"
            | _   -> failwith ("Bad key: " ^ current_key)
            end
        | D when List.mem current_key ["D";"A";"5";"C";"9"] -> current_key
        | D -> begin
            match current_key with
                         | "1" -> "3"
            | "2" -> "6" | "3" -> "7" | "4" -> "8"
            | "6" -> "A" | "7" -> "B" | "8" -> "C"
                         | "B" -> "D"
            | _   -> failwith ("Bad key: " ^ current_key)
            end
        | L when List.mem current_key ["1";"2";"5";"A";"D"] -> current_key
        | L -> begin
            match current_key with
                         | "3" -> "2" | "4" -> "3"
            | "6" -> "5" | "7" -> "6" | "8" -> "7" | "9" -> "8"
                         | "B" -> "A" | "C" -> "B" 
            | _   -> failwith ("Bad key: " ^ current_key)
            end
        | R when List.mem current_key ["1";"4";"9";"C";"D"] -> current_key
        | R -> begin
            match current_key with
                         | "2" -> "3" | "3" -> "4"
            | "5" -> "6" | "6" -> "7" | "7" -> "8" | "8" -> "9"
                         | "A" -> "B" | "B" -> "C" 
            | _   -> failwith ("Bad key: " ^ current_key)
            end
 

end

let read_day2() : Keypad.direction list list =
    AdventStdLib.read_lines_from_file "../inputs/day2.txt"
    |> List.map AdventStdLib.explode_str
    |> List.map (List.map Keypad.parse_direction)

let day2a() : int list =
    read_day2()
    |> List.map (List.fold_left Keypad.move_on_simple_keypad 5) 

let day2b() : string list =
    read_day2()
    |> List.map (List.fold_left Keypad.move_on_complex_keypad "5") 

let () =
    let day2a_code = day2a() |> List.map string_of_int |> String.concat "" in
    let day2b_code = day2b() |> String.concat "" in
    Printf.printf "Day 2A: %s\n" day2a_code;
    Printf.printf "Day 2B: %s\n" day2b_code

