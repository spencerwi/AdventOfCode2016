module GridScreen = struct
    type pixel = {x: int; y: int; on: bool}
    type t = pixel list

    let screen_height = 6
    let screen_width = 50 
    let initial_screen : t = 
        AdventStdLib.int_range 0 screen_height ~step:1
        |> List.map (fun y -> 
            AdventStdLib.int_range 0 screen_width ~step:1
            |> List.map (fun x -> {x;y; on=false}) 
        )
        |> List.flatten

    let rect width height screen =
        screen 
        |> List.map (fun {x;y;on} -> 
            let new_on = on || (x < width && y < height) in
            {x;y;on = new_on}
        )

    let rotate_column col amount screen =
        let is_in_column pixel = pixel.x = col in
        screen
        |> List.map (function 
            | p when is_in_column p -> 
                let new_y = (p.y + amount) mod screen_height in
                {p with y = new_y}
            | p -> p
        )

    let rotate_row row amount screen =
        let is_in_row pixel = pixel.y = row in
        screen
        |> List.map (function 
            | p when is_in_row p -> 
                let new_x = (p.x + amount) mod screen_width in
                {p with x = new_x}
            | p -> p
        )

    let sort screen =
        screen
        |> List.sort (fun a b ->
            match compare a.y b.y with
            | 0 -> compare a.x b.x
            | n -> n
        )

    let to_string screen =
        let rows = 
            sort screen
            |> AdventStdLib.group_by_comparison (fun a b -> compare a.y b.y) 
        in
        let row_to_string row = 
            row
            |> List.map (function
                | {x;y;on} when on = true  -> "#"
                | {x;y;on} when on = false -> "."
            )
            |> String.concat ""
        in
        rows
        |> List.map row_to_string
        |> String.concat "\n"

    module Instruction = struct
        type t =
            | Rect of {width: int; height: int}
            | RotateColumn of {col: int; amount: int}
            | RotateRow of {row: int; amount: int}

        let parse s =
            let open Angstrom in
            let integer = 
                take_while1 (function 
                    | '0' .. '9' -> true 
                    | _ -> false
                ) >>| int_of_string
            in
            let parse_rect = 
                (string "rect ") *> (sep_by1 (char 'x') integer)
                >>| (fun [width;height] -> Rect {width; height}) 
            in
            let parse_rotate_column = 
                (string "rotate column x=") *> (sep_by1 (string " by ") integer)
                >>| (fun [col;amount] -> RotateColumn {col;amount}) 
            in
            let parse_rotate_row = 
                (string "rotate row y=") *> (sep_by1 (string " by ") integer)
                >>| (fun [row;amount] -> RotateRow {row;amount}) 
            in
            let instruction = choice [parse_rect;parse_rotate_row;parse_rotate_column] in
            match parse_only instruction (`String s) with
            | Result.Ok i -> i
            | Result.Error msg -> failwith msg

        let apply screen = function
            | Rect {width;height} -> rect width height screen
            | RotateColumn {col;amount} -> rotate_column col amount screen
            | RotateRow {row;amount} -> rotate_row row amount screen
    end
end




let read_day8 () : GridScreen.Instruction.t list =
    AdventStdLib.read_lines_from_file "../inputs/day8.txt"
    |> List.map GridScreen.Instruction.parse

let repl () =
    let rec repl' screen =
        print_endline "\n";
        print_endline (GridScreen.to_string screen);
        print_string "Enter a command: ";
        let command = read_line() in
        match command with 
        | "exit" -> print_endline "\nQuitting."
        | _ -> begin
            let new_screen = 
                GridScreen.Instruction.parse command
                |> GridScreen.Instruction.apply screen
            in
            repl' new_screen
        end
    in
    repl' GridScreen.initial_screen


let day8() : (int * GridScreen.t) =
    let final_screen = 
        read_day8()
        |> List.fold_left GridScreen.Instruction.apply GridScreen.initial_screen 
    in
    let pixels_on = 
        final_screen
        |> List.filter (fun pixel -> pixel.GridScreen.on)
        |> List.length
    in
    (pixels_on, final_screen)

let () =
    let (pixels_on, final_screen) = day8() in
    Printf.printf "Day 8A: %d\n" pixels_on;
    Printf.printf "Day 8B: \n%s\n" (GridScreen.to_string final_screen)
