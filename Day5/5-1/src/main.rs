extern crate crypto;

use crypto::md5::Md5;
use crypto::digest::Digest;

fn hash_str(s: String) -> String {
    let mut sh = Md5::new();
    sh.input_str(&s);

    return sh.result_str();
}

fn starts_with_zeros(s: &String, cnt: usize) -> bool {
    for (i, c) in s.chars().enumerate() {
        if i == cnt { return true }
        if c != '0' { return false }
    }

    return true;
}

fn char_at(s: String, n: usize) -> Option<char> {
    for (i, c) in s.chars().enumerate() {
        if i == n { return Some(c) }
    }
    return None;
}

fn main() {
    let puzzle_input = "ugkcyxxp";
    let mut i = 0;
    let mut cnt = 0;
    let mut password = "".to_string();
    loop {
        let hash = hash_str(puzzle_input.to_string() + &i.to_string());
        if starts_with_zeros(&hash, 5) {
            match char_at(hash, 5) {
                Some(c) => {
                    password.push(c);
                    cnt += 1;
                },
                None => (),
            };
        }
        if cnt >= 8 {
            println!("{}", password);
            break;
        }
        i += 1;
    }
}
