type room = 
    {
        name: string;
        checksum: string;
        sector_id: int;
    }
let calculate_checksum (room_name: string) : string = 
    let letter_groups : string list list = 
        AdventStdLib.explode_str room_name 
        |> List.filter AdventStdLib.is_alpha
        |> AdventStdLib.group
    in
    let by_count_by_alphabetical a b =
        let result = compare (List.length b) (List.length a) in
        if result = 0 
        then compare (List.nth a 0) (List.nth b 0)
        else result 
    in
    List.sort by_count_by_alphabetical letter_groups
    |> List.map (fun l -> List.nth l 0)
    |> AdventStdLib.take 5
    |> String.concat ""

let parse_room s : room = 
    let [name_and_sector_id;checksum_and_closing_bracket] = String.split_on_char '[' s in
    let (sector_id, name) = 
        name_and_sector_id 
        |> AdventStdLib.explode_str
        |> List.partition AdventStdLib.is_digit
        |> (fun (sector_id, name) -> (sector_id |> String.concat "" |> int_of_string, name |> String.concat ""))
    in
    let checksum = (String.sub checksum_and_closing_bracket 0 ((String.length checksum_and_closing_bracket) - 1)) in
    {name; checksum; sector_id}


let is_real_room (room: room) : bool =
    room.checksum = calculate_checksum room.name

let read_day4 () : room list =
    AdventStdLib.read_lines_from_file "../inputs/day4.txt"
    |> List.map parse_room

let day4a () =
    read_day4()
    |> List.filter is_real_room
    |> List.map (fun r -> r.sector_id)
    |> List.fold_left (+) 0

let day4b () : int =
    read_day4()
    |> List.filter is_real_room
    |> List.map (fun room ->
        let translated_name = 
            room.name
            |> Str.global_replace (Str.regexp "-") " "
            |> AdventStdLib.rotate_string room.sector_id
        in
        {room with name = translated_name}
    )
    |> List.find (fun room -> AdventStdLib.str_contains room.name "northpole")
    |> (fun {sector_id} -> sector_id)

let () =
    Printf.printf "Day 4A: %d\n" (day4a());
    Printf.printf "Day 4B: %d\n" (day4b())
