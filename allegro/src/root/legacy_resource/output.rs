use super::tokens::LegacyToken;
use crate::{colored::Colorize, root::legacy_resource::tokens::TokenType};


#[macro_export]
macro_rules! quit {
    () => {
        std::process::exit(0);
    };

    ($code:ident) => {
        //println!("[q] {}", format_args!($($arg)*))
        std::process::exit($code);
    }
}

#[macro_export]
macro_rules! error_nocode {
    () => {
        use $crate::quit;
        use colored::Colorize;

        eprintln!("{} An Error Occured!", "[!]".red());
        quit!()
    };
    ($($arg:tt)*) => {
                use colored::Colorize;

        eprintln!("{} {}", "[!]".red(), format_args!($($arg)*));
        $crate::quit!();
    };
}

#[macro_export]
macro_rules! error {
    () => {
        eprintln!("{} An Error Occured!", "[!]".red());
        quit!()

    };

    ($code:path) => {
        use colored::Colorize;

        eprintln!("{} {}","[!]".red(), $code($($arg)*));
    };

    ($code:path, ($($arg:tt)*)) => {
        use crate::quit;
        use colored::Colorize;

        //use crate::core::resource::errors::Errors;
        eprintln!("{} {}","[!]".red(), $code($($arg)*));
        if $code($($arg)*).get_hint().is_some() {
            eprintln!("Hint: {}", $code($($arg)*).get_hint().unwrap());
        }
        let c: i32 = $code($($arg)*).discriminant().try_into().unwrap();
        //panic!();
        quit!(c);
    }
    
}

#[macro_export]
macro_rules! error_noquit {
    () => {
        eprintln!("{} An Error Occured!", "[!]".red());
    };

    ($code:path) => {
        use colored::Colorize;

        eprintln!("{} {}","[!]".red(), $code($($arg)*));
    };

    ($code:path, ($($arg:tt)*)) => {
        use colored::Colorize;

        //use crate::core::resource::errors::Errors;
        eprintln!("{} {}","[!]".red(), $code($($arg)*));
        if $code($($arg)*).get_hint().is_some() {
            eprintln!("\tHint: {}", $code($($arg)*).get_hint().unwrap());
        }

    }
    
}

#[macro_export]
macro_rules! errorold_no_quit {
    () => {
        use colored::Colorize;

        eprintln!("{} An Error Occured!", "[!]".red());
    };
    ($($arg:tt)*) => {
        use colored::Colorize;

        eprintln!("{} {}", "[!]".red(), format_args!($($arg)*));
    };
}

#[macro_export]
macro_rules! info {
    () => {

    };
    ($($arg:tt)*) => {
        use colored::Colorize;

        println!("{} {}", "[i]".cyan(), format_args!($($arg)*));
    }
}

pub fn draw_error(tkvec: &Vec<LegacyToken>, start: usize) {
    //let mut accum = format!("{}", "...".to_string().black());
    let mut accum = format!("");

    let mut linestart = 0;
    let mut lineend = 0;
    let mut s: Vec<LegacyToken> = tkvec.clone()[0..start].to_vec();
    let e: Vec<LegacyToken> = tkvec.clone()[start..tkvec.len()].to_vec();
    s.reverse();
    for (i, _t) in s.iter().enumerate() {
        if tkvec.get(i).unwrap().tokentype == TokenType::TkStatementEnd || tkvec.get(i).unwrap().tokentype == TokenType::TkLBrace{
            linestart = i + 1
        }
    }
    for (i, _t) in e.iter().enumerate() {
        if tkvec.get(i + start).unwrap().tokentype == TokenType::TkStatementEnd || tkvec.get(i + start).unwrap().tokentype == TokenType::TkLBrace{
            lineend= i + start + 1;
            break;
        }
    } 
    for j in linestart ..lineend {
        let c = tkvec.get(j).unwrap();
        if j == linestart || (c.tokentype == TokenType::TkLparen && tkvec.get(j).unwrap().tokentype == TokenType::TkSymbol) || c.tokentype == TokenType::TkColon || c.tokentype == TokenType::TkStatementEnd{
            accum = format!("{accum}{}", c.to_string())
        } else {
            accum = format!("{accum} {}", c.to_string())

        }
    }

    let offending_str = &tkvec.get(start - 1).unwrap().to_string();
    println!("\t{accum}");
    println!(
        "\t{char:>width$} {here}",
        char = "^".repeat(offending_str.len()).red().bold(),
        width = accum.find(offending_str).unwrap() ,
        here = "here".red().bold(),
    );
}
