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


let is_only charset s =
    explode_str s
    |> List.map (fun c -> String.get c 0)
    |> List.for_all (String.contains charset)

let lowercase_alpha = "abcdefghijklmnopqrstuvwxyz"
let digit = "0123456789"

let is_alpha s = 
    String.lowercase_ascii s
    |> is_only lowercase_alpha 

let is_digit s = is_only digit s

let group_by_comparison (comparator: 'a -> 'a -> int) (l: 'a list) : 'a list list =
    let sorted = List.sort comparator l in
    let fold first rest = List.fold_left
        (fun (acc, grouping, last) elem ->
            if (comparator last elem) = 0 
            then (acc, elem::grouping, elem)
            else (grouping::acc, [elem], elem)
        )
        ([], [first], first)
        rest
    in
    match sorted with
    | [] -> []
    | next::rest -> begin
        let groups, last_group, _ = fold next rest in
        List.rev_map List.rev (last_group::groups)
    end

let group (l: 'a list) : 'a list list = group_by_comparison (Pervasives.compare) l

let rec take n l = 
    match (n, l) with
    | 0, _  -> []
    | _, [] -> [] 
    | _, next::rest -> next::(take (n-1) rest)


let list_index_of x l =
    let rec list_index_of' i = function
    | [] -> failwith "Not found"
    | next::rest ->
        if next = x 
        then i 
        else list_index_of' (i+1) rest
    in
    list_index_of' 0 l

let rec rotate_list n l =
    match n, l with
    | 0, _  -> l
    | _, [] -> []
    | _, next::rest -> begin
        let rotated_once = rest @ [next] in
        rotate_list (n - 1) rotated_once
    end

let rotate_string n s =
    if n = 0 then
        s
    else begin
        let original_alpha = lowercase_alpha |> explode_str in
        let rotated_alpha = original_alpha |> (rotate_list n) in
        String.lowercase_ascii s
        |> explode_str
        |> List.map (fun c -> 
            if is_alpha c 
            then  
                let original_alpha_position = list_index_of c original_alpha in
                List.nth rotated_alpha original_alpha_position
            else c
        )
        |> String.concat ""
    end

let str_contains s1 s2 =
    let regexp = Str.regexp s2 in
    match (Str.search_forward regexp s1 0) with 
    | exception Not_found -> false
    | _ -> true

