type turn_direction = L | R
type compass_direction = N | S | E | W
type state = {x: int; y: int; facing: compass_direction}
type move = {turn: turn_direction; steps: int}

let parse_direction = function
    | 'L' -> L
    | 'R' -> R
    | _   -> failwith "Bad direction!"
let parse_move (s: string): move =
    let turn_direction: turn_direction = (String.get s 0 |> parse_direction) in
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


let read_day1 () : move list =
    AdventStdLib.read_lines_from_file "../inputs/day1.txt"
    |> (fun l -> List.nth l 0)
    |> Str.split (Str.regexp ", ")
    |> List.map parse_move

let day1A () = 
    let initial_state = {x=0; y=0; facing=N} in
    let final_state = 
        read_day1()
        |> List.fold_left apply_move initial_state
    in
    let distance = (abs final_state.x) + (abs final_state.y) 
    in distance

let day1B () =
    0 (* TODO: this *)
    (* Use a recursive function with an accumulator array, 
     * stop when the step would have you in the same place again, and then return that place.
     * Then get the taxicab distance *)

let () = 
    let day1A_result = day1A() in
    let day1B_result = day1B() in
    Printf.printf "Day1A: %d\n" day1A_result;
    Printf.printf "Day1B: %d\n" day1B_result
