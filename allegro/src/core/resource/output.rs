use colored::Colorize;

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
macro_rules! error {
    () => {
        use crate::quit;
        use colored::Colorize;

        eprintln!("{} An Error Occured!", "[!]".red());
        quit!()
    };
    ($($arg:tt)*) => {
        
        use colored::Colorize;

        eprintln!("{} {}", "[!]".red(), format_args!($($arg)*));
        crate::quit!();
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
