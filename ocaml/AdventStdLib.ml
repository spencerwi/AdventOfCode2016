let read_lines_from_file (filename: string) : string list =
    let lines = ref [] in
    let chan = open_in filename in
    try
        while true; do
            lines := (input_line chan) :: !lines
        done;
        !lines
    with End_of_file -> 
        close_in chan;
        List.rev !lines

let rec int_range (start: int) (stop: int) ?step:(s=1) : int list =
    if start = stop 
    then []
    else 
        match start > stop with
        | true  when s < 0 -> start :: (int_range (start + s) stop ~step:s)
        | false when s > 0 -> start :: (int_range (start + s) stop ~step:s)
        | _ -> []

let rec explode_str = function
    | "" -> []
    | s  -> (String.sub s 0 1) :: explode_str (String.sub s 1 (String.length s - 1))

