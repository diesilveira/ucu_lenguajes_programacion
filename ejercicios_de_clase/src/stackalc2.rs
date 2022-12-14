use std::{io, f64::NAN};
use try_catch::catch;



pub fn stackalc2() {
    // Declare 10 variables
    let mut variables = Vec::new();
    let mut value = 0;
    while value < 10 {
        variables.push(NAN);
        value = value + 1;
    }
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
                break;
            } else {
                push_read_line_into_stack(line.clone(), &mut list, &mut variables);
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

fn push_read_line_into_stack(line: String, list: &mut Vec<f64>, variables: &mut Vec<f64>) {
    let mut list_read: Vec<&str>;
    list_read = line.trim().split_whitespace().collect::<Vec<_>>();
    list_read.reverse();
    while list_read.len() != 0 {
        catch! {
            try {
                let mut value: &str = &list_read.pop().unwrap().to_uppercase();
                let values: Vec<&str> = value.split(":").collect();
                let mut position = 0;
                if values.len() > 1 {
                    value = values[0];
                    position = values[1].parse().expect("Not a number");
                }
                match value {
                    "ADD" => add(list),
                    "SUB" => sub(list),
                    "MULT" => mult(list),
                    "EQ" => eq(list),
                    "DIFF" => diff(list),
                    "LT" => lt(list),
                    "LTE" => lte(list),
                    "GT" => gt(list),
                    "GTE" => gte(list),
                    "NOT" => not(list),
                    "AND" => and(list),
                    "OR" => or(list),
                    "GET" => get_value(list, (variables).to_vec(), position),
                    "SET" => set_value(list, variables, position),
                    "POP" => println!("{}", list.pop().unwrap()),
                    "DUP" => dup(list),
                    _ =>  {
                        let _number : f64 = value.trim().parse()?;
                        list.push(_number);
                    }
                }
            } catch parse_float_error {
                println!("Can't push in the stack: {}.", parse_float_error)
            }
        }
    }
    print_variables(&mut variables.clone());
    print_list(&mut list.clone());
}

fn print_list(list: &mut Vec<f64>) {
    list.reverse();
    print!("[");
    while list.len() > 1 {
        print!("{},", list.pop().unwrap());
    }
    if list.len() == 1 {
        print!("{}", list.pop().unwrap());
    }
    print!("]");
    println!("");
}

fn print_variables(list: &mut Vec<f64>) {
    print!("Variables: [");
    while list.len() > 1 {
        print!("{},", list.pop().unwrap());
    }
    if list.len() == 1 {
        print!("{}", list.pop().unwrap());
    }
    print!("]");
    println!("");
}

fn mult(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    list.push(first * second);
}

fn add(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    list.push(second + first);
}

fn sub(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    list.push(second - first);
}

fn eq(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if first == second {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn diff(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if first != second {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn lt(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if second < first  {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn lte(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if second <= first {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn gt(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if second > first {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn gte(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if second >= first {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn not(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    if first != 0.0 || first != NAN {
        list.push(0.0);
    } else {
        list.push(1.0);
    }
}

fn and(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if (first != 0.0) && (second != 0.0) {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn or(list: &mut Vec<f64>) {
    let first = list.pop().unwrap();
    let second = list.pop().unwrap();
    if (first != 0.0) || (second != 0.0) {
        list.push(1.0);
    } else {
        list.push(0.0);
    }
}

fn get_value(list: &mut Vec<f64>, variables: Vec<f64>, position: usize) {
    let value_to_add: f64 = variables[position];
    list.push(value_to_add);
}

fn set_value(list: &mut Vec<f64>, variables:&mut Vec<f64>, position: usize) {
    let value_to_save = list.pop().unwrap();
    variables[position] = value_to_save;
}

fn dup(list: &mut Vec<f64>) {
    let value_to_save = list.pop().unwrap();
    list.push(value_to_save);
    list.push(value_to_save);
}