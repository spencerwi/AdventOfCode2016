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
    let found = ref 0 in
    let results = ref [] in
    let i = ref 0 in
    while (!found < 8) do
        let hashed = hash_with_num !i in
        if starts_with_five_zeroes hashed 
        then begin
            results := ((get_sixth_char hashed)::(!results));
            found := (!found) + 1
        end;
        i := (!i) + 1
    done;
    String.concat "" !results

let day5b () = 
    (* TODO: This! *)
    ""

let () =
    Printf.printf "Day 5A: %s\n" (day5a());
    Printf.printf "Day 5B: %s\n" (day5b())
