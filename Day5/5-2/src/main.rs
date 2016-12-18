extern crate crypto;

use crypto::md5::Md5;
use crypto::digest::Digest;

const LEADING_ZEROS: usize = 5;
const PASS_POS_POS: usize = LEADING_ZEROS;
const PASS_CHAR_POS: usize = LEADING_ZEROS + 1;
const PASS_LEN: usize = 8;

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

        if starts_with_zeros(&hash, LEADING_ZEROS) {
            let d = char_at(&hash, PASS_POS_POS)
                .unwrap_or_default()
                .to_digit(16)
                .unwrap_or_default() as usize;

            if d < PASS_LEN && pass_holder[d] == ' ' {
                pass_holder[d] = char_at(&hash, PASS_CHAR_POS).unwrap_or_default();
                cnt += 1;
            }
        }

        if cnt >= PASS_LEN {
            for c in &pass_holder {
                password.push(*c);
            }
            println!("{}", password);

            break;
        }

        i += 1;
    }
}
