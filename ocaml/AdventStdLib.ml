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
        match (s > 0, start > stop) with
        | true, true  -> []
        | true, false -> start :: (int_range (start + s) stop ~step:s)
        | false, true -> start :: (int_range (start + s) stop ~step:s)
        | false, false -> []

