
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
macro_rules! lexingerror {
    () => {
        eprintln!("[!] An error occurred!");
        quit!()
    };
    ($($arg:tt)*) => {
        eprintln!("[!] {}", format_args!($($arg)*));
        quit!();
    }
}
