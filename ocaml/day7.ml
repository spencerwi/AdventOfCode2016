module IP7Addr = struct
    type t = 
        {
            bracketed_sections: string list;
            unbracketed_sections: string list;
        }
    let parse s =
        let open Angstrom in
        let section = take_while1 (fun x -> x <> '[' && x <> ']') in
        let unbracketed_section = section >>| (fun s -> `Unbracketed s) in
        let bracketed_section   = (char '[' *> section <* char ']') >>| (fun s -> `Bracketed s) in
        let sections = many (bracketed_section <|> unbracketed_section)
        in
    match parse_only sections (`String s) with
    | Result.Ok sections -> 
        let bracketed_sections = 
            sections 
            |> List.filter (function
                | `Bracketed _ -> true
                | _ -> false
            )
            |> List.map (function
                |`Bracketed s -> s
                | _ -> failwith "shouldn't be here!"
            )
        in
        let unbracketed_sections = 
            sections 
            |> List.filter (function
                | `Unbracketed _ -> true
                | _ -> false
            )
            |> List.map (function
                |`Unbracketed s -> s
                | _ -> failwith "shouldn't be here!"
            )
        in
        { bracketed_sections; unbracketed_sections }
    | Result.Error msg -> failwith msg

    let supports_tls addr =
        let contains_abba s =
            let chars = AdventStdLib.explode_str s in
            let rec check = function
                | a::(b::c::d::rest as next_set) -> 
                        begin
                            if a = d && b = c  && a <> b
                    then true
                    else check next_set
                        end
                | _ -> false
            in
            check chars
        in
        List.exists contains_abba addr.unbracketed_sections
        &&
        List.for_all (fun s -> not @@ contains_abba s) addr.bracketed_sections

    let supports_ssl ip7_addr =
        let get_abas s =
            let chars = AdventStdLib.explode_str s in
            let rec get_aba = function
                | a::(b::c::rest as next_set) -> 
                    begin
                        if a = c && a <> b 
                        then 
                            let aba = a ^ b ^ a in
                            aba :: (get_aba next_set)
                        else 
                            get_aba next_set
                    end
                | _ -> []
            in
            get_aba chars
        in
        let abas = 
            ip7_addr.unbracketed_sections
            |> List.map get_abas
            |> List.flatten
        in
        let babs = 
            ip7_addr.bracketed_sections
            |> List.map get_abas
            |> List.flatten
        in
        if abas = []
        then false
        else
            abas 
            |> List.exists (fun aba ->
                let bab_for_aba = 
                    let a = String.get aba 0 |> String.make 1 in
                    let b = String.get aba 1 |> String.make 1 in
                    b ^ a ^ b
                in
                List.mem bab_for_aba babs
           )
end

let read_day7 () : IP7Addr.t list =
    AdventStdLib.read_lines_from_file "../inputs/day7.txt"
    |> List.map IP7Addr.parse

let day7a () =
    read_day7()
    |> List.filter IP7Addr.supports_tls
    |> List.length

let day7b () =
    read_day7()
    |> List.filter IP7Addr.supports_ssl
    |> List.length

let () =
    Printf.printf "Day 7A: %d\n" (day7a());
    Printf.printf "Day 7B: %d\n" (day7b());
