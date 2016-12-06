let is_valid_triangle side1 side2 side3 =
    (
        ((side1 + side2) > side3)
        &&
        ((side2 + side3) > side1)
        &&
        ((side3 + side1) > side2)
    )

let read_day3 () : (int * int * int) list =
    let parse_row row =
        let [a;b;c] = 
            Str.split (Str.regexp " +") row 
            |> List.map int_of_string
        in
        (a,b,c)
    in
    AdventStdLib.read_lines_from_file "../inputs/day3.txt"
    |> List.map parse_row

let day3a () : int =
    read_day3()
    |> List.filter (fun (a,b,c) -> is_valid_triangle a b c)
    |> List.length

let () =
    Printf.printf "Day 3A: %d\n" (day3a())
