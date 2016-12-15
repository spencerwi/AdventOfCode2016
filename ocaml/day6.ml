let read_day6() =
    AdventStdLib.read_lines_from_file "../inputs/day6.txt"
    |> List.map AdventStdLib.explode_str
    |> BatList.transpose

type desired_frequency = MostCommon | LeastCommon

let find_char_for_column desired_frequency column =
    let sorted_by_frequency = 
        column
        |> BatList.group Pervasives.compare 
        |> List.map (fun letter_group -> (List.nth letter_group 0, List.length letter_group))
        |> BatList.sort (fun (_, countA) (_, countB) -> compare countB countA) 
    in
    let extract_letter (c, _) = c in
    match desired_frequency with
    | MostCommon -> List.nth sorted_by_frequency 0 |> extract_letter
    | LeastCommon -> BatList.last sorted_by_frequency |> extract_letter

let day6a() =
    read_day6()
    |> List.map (find_char_for_column MostCommon)
    |> String.concat ""

let day6b () = 
    read_day6()
    |> List.map (find_char_for_column LeastCommon)
    |> String.concat ""


let () =
    Printf.printf "Day 6A: %s\n" (day6a());
    Printf.printf "Day 6B: %s\n" (day6b());


