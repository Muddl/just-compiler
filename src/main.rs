#![allow(warnings)]

mod front;
mod define;

use front::{lexer::Lexer};
use define::Token::{Key, End};
use std::fs::File;
use std::env;
use std::path::Path;

fn main() {
    // parse args
    let args: Vec<_> = env::args().collect();

    let file = File::open(&args[1]).expect("Unable to open");

    let file_name = Path::new(&args[1]).file_name().unwrap();

    let mut lexer = Lexer::new(file);

    println!("File Name: {:?}\n", file_name);
    println!("Tokens");
    println!("----------");

    let mut cur_token = lexer.next_token();

    while cur_token != Ok(End) {
        println!("{}", cur_token.unwrap());
        cur_token = lexer.next_token();
    }

    println!("{}", cur_token.unwrap());
}
