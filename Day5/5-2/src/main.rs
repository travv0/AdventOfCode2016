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

fn char_at(s: &String, n: usize) -> Option<char> {
    for (i, c) in s.chars().enumerate() {
        if i == n { return Some(c) }
    }
    return None;
}

fn main() {
    let puzzle_input = "ugkcyxxp";
    let mut i = 0;
    let mut cnt = 0;
    let mut pass_holder: [char; 8] = [' '; 8];
    let mut password = "".to_string();

    loop {
        let hash = hash_str(puzzle_input.to_string() + &i.to_string());

        if starts_with_zeros(&hash, 5) {
            let d = char_at(&hash, 5)
                .unwrap_or_default()
                .to_digit(16)
                .unwrap_or_default() as usize;

            if d < 8 && pass_holder[d] == ' ' {
                pass_holder[d] = char_at(&hash, 6).unwrap_or_default();
                cnt += 1;
            }
        }

        if cnt >= 8 {
            for c in &pass_holder {
                password.push(*c);
            }
            println!("{}", password);

            break;
        }

        i += 1;
    }
}
