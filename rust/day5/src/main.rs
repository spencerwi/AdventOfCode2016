extern crate crypto;

use crypto::md5::Md5;
use crypto::digest::Digest;


fn starts_with_five_zeroes(hashed: [u8; 16]) -> bool {
    // We're dealing with u8, which means 8-bit unsigned ints.
    // So each int is a two-digit pair.
    // 00               00                 0 (we have to shift by 4 bits to get just the one digit)
    (hashed[0] as i32 + hashed[1] as i32 + (hashed[2] as i32 >> 4)) == 0
}

fn nth_char(s: String, n: usize) -> char {
    s.chars().nth(n).unwrap()
}

fn day5a(prefix: &str){
    print!("Day 5A: ");
    let mut hasher = Md5::new();
    let mut found = 0;
    let mut i = 0;
    while found < 8 {
        let mut output = [0; 16];
        hasher.input(prefix.as_bytes());
        hasher.input(i.to_string().as_bytes());
        hasher.result(&mut output);
        if starts_with_five_zeroes(output) {
            let sixth_char = nth_char(hasher.result_str(), 5);
            print!("{}", sixth_char);
            found += 1;
        }
        hasher.reset();
        i += 1;
        if found == 8 {
            println!("");
        }
    }
}
fn day5b(prefix: &str){
    print!("Day 5B: ");
    let mut hasher = Md5::new();
    let mut done = false;
    let mut i = 0;
    let mut password = ['_'; 8];
    while !done {
        let mut output = [0; 16];
        hasher.input(prefix.as_bytes());
        hasher.input(i.to_string().as_bytes());
        hasher.result(&mut output);
        if starts_with_five_zeroes(output) {
            let position = nth_char(hasher.result_str(), 5).to_digit(10);
            match position {
                Some(p) => {
                    if (p < 8) && (p >= 0) && (password[p as usize] == '_') {
                        let password_char = nth_char(hasher.result_str(), 6);
                        password[p as usize] = password_char;
                    }
                },
                None => {}
            }
        }
        hasher.reset();
        i += 1;
        done = password.iter().cloned().all(|c| c != '_');
        if done {
            let password_str: String = password.iter().cloned().collect();
            println!("{}", password_str);
        }
    }
}

fn main() {
    let prefix = "wtnhxymk";
    day5a(prefix);
    day5b(prefix);
}

