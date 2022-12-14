use std::io;
use try_catch::catch;

pub fn stackalc() {
    println!("Tap doubles & tap enter to push, when you want pop only tap enter.");
    let mut line = String::new();
    let mut list: Vec<f64> = Vec::new();
    loop {
        io::stdin()
            .read_line(&mut line)
            .expect("Ups... We have a problem reading");
        if should_print_empty_stack(list.clone(), line.clone()) {
            empty_stack();
            break;
        } else {
            if should_pop_stack(line.clone()) {
                println!("Pop: {}", list.pop().unwrap());
            } else {
                push_read_line_into_stack(line.clone(), &mut list);
                line.clear();
            }
        }
    }
}

fn empty_stack() {
    println!("Empty stack.");
}

fn should_print_empty_stack(list: Vec<f64>, read_line: String) -> bool {
    return read_line.trim().is_empty() && list.len() == 0;
}

fn should_pop_stack(read: String) -> bool {
    return read.trim().is_empty();
}

fn push_read_line_into_stack(line: String, list: &mut Vec<f64>) {
    let mut list_read: Vec<&str>;
    list_read = line.trim().split_whitespace().collect::<Vec<_>>();
    list_read.reverse();
    while list_read.len() != 0 {
        catch! {
            try {
                let _number : f64 = list_read.pop().expect("Can't pop").trim().parse()?;
                list.push(_number);
            } catch parse_float_error {
                println!("Can't push in the stack: {}.", parse_float_error)
            }
        }
    }
}
