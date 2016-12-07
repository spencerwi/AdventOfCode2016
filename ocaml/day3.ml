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

let day3b () : int =
    let rec read_columns_by_threes = function
        | [] -> []
        | one::two::three::rest ->
            let (a1, b1, c1) = one in
            let (a2, b2, c2) = two in
            let (a3, b3, c3) = three in
            [(a1, a2, a3); (b1, b2, b3); (c1, c2, c3)] @ (read_columns_by_threes rest)
    in
    read_day3()
    |> read_columns_by_threes
    |> List.filter (fun (a,b,c) -> is_valid_triangle a b c) 
    |> List.length

let () =
    Printf.printf "Day 3A: %d\n" (day3a());
    Printf.printf "Day 3B: %d\n" (day3b())
