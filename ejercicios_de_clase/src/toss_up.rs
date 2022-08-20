use rand::Rng;
use std::io::{self, BufRead}; // https://docs.rs/rand

pub fn toss_up() {
    let mut options = vec![];
    println!("Your options please:");
    let mut lines = io::stdin().lock().lines(); // Line iterator
    while let Some(line) = lines.next() {
        // For each line
        let line_str = line.unwrap();
        if line_str.len() == 0 {
            // If line empty, exit loop.
            break;
        }
        options.push(line_str); // Collect nonempty line.
    }
    let random_index = rand::thread_rng().gen_range(0..=(options.len() - 1)); // Random array index
    println!("Chosen: {}", options[random_index]); // Show
}
