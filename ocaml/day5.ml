let day5_input = "wtnhxymk"

let hash_with_num n =
    (day5_input ^ (string_of_int n))
    |> Digest.bytes
    |> Digest.to_hex


let day5a () = 
    let starts_with_five_zeroes hash =
        (String.sub hash 0 4) = "00000"
    in
    let get_sixth_char hash = 
        (String.get hash 5) |> String.make 0 
    in
    BatLazyList.from_loop 0 (fun i -> hash_with_num i, i + 1)
    |> BatLazyList.filter starts_with_five_zeroes
    |> BatLazyList.map get_sixth_char
    |> BatLazyList.take 8
    |> BatLazyList.to_list
    |> String.concat ""

let day5b () = 
    (* TODO: This! *)
    ""

let () =
    Printf.printf "Day 5A: %s\n" (day5a());
    Printf.printf "Day 5B: %s\n" (day5b())
