use super::tokens::Token;
use crate::colored::Colorize;


#[macro_export]
macro_rules! quit {
    () => {
        std::process::exit(0);
    };

    ($($arg:tt)*) => {
        println!("[q] {}", format_args!($($arg)*))
        std::process::exit(1);
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
        quit!();
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
        use crate::quit;
        use colored::Colorize;

        //use crate::core::resource::errors::Errors;
        eprintln!("{} {}","[!]".red(), $code($($arg)*));
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

pub fn draw_error(tkvec: &Vec<Token>, start: usize) {
    let mut accum = format!("{}", "... ".to_string().black());
    for i in start - 4..=start + 3 {
        accum = format!("{accum}{}", tkvec.get(i).unwrap().to_string())
    }
    accum = format!("{accum} {}", "...".black());

    let offending_str = &tkvec.get(start).unwrap().to_string();
    //println!("| ");
    println!("{} {}", "|".blue(),
    accum);
    println!(
        "{pipe} {char:>width$} {here}",
        pipe = "|".blue(),
        char = "^".repeat(offending_str.len()).red().bold(),
        width = accum.find(offending_str).unwrap() + 3,
        here = "here".red().bold(),
    );
}
