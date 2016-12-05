module Grid = struct
    type turn_direction = L | R
    type compass_direction = N | S | E | W
    type state = {x: int; y: int; facing: compass_direction}
    type move = {turn: turn_direction; steps: int}

    let initial_state: state = {x=0; y=0; facing=N}

    let parse_turn_direction = function
        | 'L' -> L
        | 'R' -> R
        | _   -> failwith "Bad direction!"

    let parse_move (s: string): move =
        let turn_direction: turn_direction = (String.get s 0 |> parse_turn_direction) in
        let steps: int = (String.sub s 1 ((String.length s) - 1) |> int_of_string) in
        {turn = turn_direction; steps}

    let apply_move (current_state: state) (m: move) =
        let now_facing = 
            match (current_state.facing, m.turn) with
            | (N, L) | (S, R) -> W
            | (N, R) | (S, L) -> E
            | (E, L) | (W, R) -> N
            | (E, R) | (W, L) -> S
        in
        let (newX, newY) = 
            match now_facing with
            | N -> (current_state.x, current_state.y + m.steps)
            | S -> (current_state.x, current_state.y - m.steps)
            | E -> (current_state.x + m.steps, current_state.y)
            | W -> (current_state.x - m.steps, current_state.y)
        in
        { facing = now_facing; x = newX; y = newY}

    let is_same_location (state1: state) (state2: state) : bool =
        (state1.x = state2.x) && (state1.y = state2.y)

    let blocks_between (state1: state) (state2: state) : int =
        (abs (state2.x - state1.x)) + (abs (state2.y - state1.y))

    let string_of_state ({x;y}) = Printf.sprintf "{x: %d, y: %d}" x y
end


let read_day1 () : Grid.move list =
    AdventStdLib.read_lines_from_file "../inputs/day1.txt"
    |> (fun l -> List.nth l 0)
    |> Str.split (Str.regexp ", ")
    |> List.map Grid.parse_move

let day1A () = 
    let final_state = 
        read_day1()
        |> List.fold_left Grid.apply_move Grid.initial_state
    in
    Grid.blocks_between Grid.initial_state final_state

let day1B () =
    let moves = read_day1() in
    let rec step (seen_states: Grid.state list) (current_state: Grid.state) = function
        | [] -> None
        | next_move::rest -> 
            let new_state = Grid.apply_move current_state next_move in
            let has_seen_state = List.exists (Grid.is_same_location new_state) seen_states in
            (* Printf.printf "current_state: %s\n" (Grid.string_of_state current_state); *)
            (* Printf.printf "new_state: %s\n" (Grid.string_of_state new_state); *)
            (* Printf.printf "seen_state: %s\n" (List.map Grid.string_of_state seen_states |> String.concat "\n\t "); *)
            (* Printf.printf "has_seen_state: %b\n" has_seen_state; *)
            if has_seen_state
            then Some new_state
            else step (new_state :: seen_states) new_state rest
    in
    step [Grid.initial_state] Grid.initial_state moves
    |> (function 
            | None -> failwith "Something went wrong; we didn't visit any state twice"
            | Some final_state -> Grid.blocks_between Grid.initial_state final_state)

let () = 
    let day1A_result = day1A() in
    let day1B_result = day1B() in
    Printf.printf "Day1A: %d\n" day1A_result;
    Printf.printf "Day1B: %d\n" day1B_result
