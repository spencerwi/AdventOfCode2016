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
    (abs final_state.Grid.x) + (abs final_state.Grid.y) 

let day1B () =
    let moves = read_day1() in
    let rec step (seen_states: Grid.state list) (current_state: Grid.state) = function
        | [] -> None
        | next_move::rest -> 
            let new_state = Grid.apply_move current_state next_move in
            let has_seen_state = List.exists (fun s -> s.Grid.x = new_state.Grid.x && s.Grid.y = new_state.Grid.y) seen_states in
            if has_seen_state
            then Some new_state
            else step (new_state :: seen_states) new_state rest
    in
    step [Grid.initial_state] Grid.initial_state moves
    |> (function 
            | None -> 0
            | Some final_state -> (abs final_state.Grid.x) + (abs final_state.Grid.y))

let () = 
    let day1A_result = day1A() in
    let day1B_result = day1B() in
    Printf.printf "Day1A: %d\n" day1A_result;
    Printf.printf "Day1B: %d\n" day1B_result
