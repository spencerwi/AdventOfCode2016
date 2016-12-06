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

    let states_between (state1: state) (state2: state) : state list =
        match state2.facing with
        | N | S -> 
            begin
                let step = 
                    if state1.y > state2.y
                    then -1
                    else 1
                in
                AdventStdLib.int_range (state1.y + step) state2.y ~step |> List.map (fun y -> {facing = state2.facing; x = state2.x; y})
            end
        | E | W ->
            begin
                let step = 
                    if state1.x > state2.x
                    then -1
                    else 1
                in
                AdventStdLib.int_range (state1.x + step) state2.x ~step |> List.map (fun x -> {facing = state2.facing; y = state2.y; x})
            end
            

    let blocks_between (state1: state) (state2: state) : int =
        (abs (state2.x - state1.x)) + (abs (state2.y - state1.y))

    let string_of_state ({facing;x;y}) = 
        let facing_str = match facing with
            | N -> "N"
            | S -> "S"
            | E -> "E"
            | W -> "W"
        in
        Printf.sprintf "%s(%d,%d)" facing_str x y
    let string_of_move ({turn;steps}) =
        let turn_str = match turn with
            | L -> "L"
            | R -> "R"
        in
        Printf.sprintf "%s%d" turn_str steps

    module StateSet = Set.Make(struct
        type t = state
        let compare s1 s2 = 
            if s1.x = s2.x && s1.y = s2.y 
            then 0
            else compare (string_of_state s1) (string_of_state s2)
    end)
end


let read_day1 () : Grid.move list =
    AdventStdLib.read_lines_from_file "../inputs/day1.txt"
    |> (fun l -> List.nth l 0)
    (* "R8, R4, R4, R8" *)
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
    let rec find_twice_visited_spot (seen_states: Grid.StateSet.t) (current_state: Grid.state) = function
        | [] -> None
        | next_move::rest -> 
            let new_state = Grid.apply_move current_state next_move in
            let states_traversed = Grid.states_between current_state new_state in
            (* Printf.printf "%s-->" (Grid.string_of_state current_state); *)
            (* List.map Grid.string_of_state states_traversed |> String.concat "-->" |> print_string; *)
            (* Printf.printf "-->%s" (Grid.string_of_state new_state); *)
            let has_seen_spot = List.exists (fun state -> Grid.StateSet.mem state seen_states) states_traversed in
            if has_seen_spot
            then begin
                (* print_endline ""; *)
                let state_already_seen = List.find (fun state -> Grid.StateSet.mem state seen_states) states_traversed in
                Some state_already_seen
            end
            else begin
                (* print_string "-->"; *)
                find_twice_visited_spot (Grid.StateSet.union seen_states (Grid.StateSet.of_list (states_traversed @ [new_state]))) new_state rest
            end
    in
    find_twice_visited_spot (Grid.StateSet.singleton Grid.initial_state) Grid.initial_state moves
    |> (function 
            | None -> failwith "Something went wrong; we didn't visit any state twice"
            | Some final_state -> Grid.blocks_between Grid.initial_state final_state)

let () = 
    let day1A_result = day1A() in
    let day1B_result = day1B() in
    Printf.printf "Day1A: %d\n" day1A_result;
    Printf.printf "Day1B: %d\n" day1B_result
