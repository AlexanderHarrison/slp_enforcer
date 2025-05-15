
pub use slp_enforcer::*;

fn main() -> std::process::ExitCode {
    let path = match std::env::args().nth(1) {
        Some(path) => path,
        None => {
            print_usage();
            return std::process::ExitCode::FAILURE;
        }
    };
    
    use slp_parser::SlpError::*;
    let game = match slp_parser::read_game(std::path::Path::new(&path)) {
        Ok((game, _)) => game,
        Err(e) => {
            eprintln!("Error opening {}:", path);
            match e {
                OutdatedFile => eprintln!(
                    "Slp file is too old! Slp enforcer requires a version >= v{}.{}.{}",
                    slp_parser::MIN_VERSION_MAJOR, slp_parser::MIN_VERSION_MINOR, 0,
                ),
                TooNewFile => eprintln!("Slp file is too new! Please update slp enforcer."),
                NotAnSlpFile => eprintln!("File is not a valid slp or slpz file."),
                InvalidFile(_) => eprintln!("File is corrupt or malformed."),
                ZstdInitError => eprintln!("Failed to initialize zstd decompressor."),
                FileDoesNotExist => eprintln!("File does not exist."),
                IOError => eprintln!("Could not open or read file.")
            }
            
            return std::process::ExitCode::FAILURE;
        }
    };
    
    let violations = check_game(&game);
    if violations.checked == 0 {
        println!(
            "no checks performed: version {}.{}.{} is too old.",
            game.info.version_major,
            game.info.version_minor,
            game.info.version_patch,
        );
    } else {
        for ply in 0..4 {
            if game.frames[ply].is_none() { continue; }
            let checked = violations.players[ply].checked;
            let found = violations.players[ply].found;
            let mut name = String::new();
            slp_parser::decode_shift_jis(&game.info.names[ply], &mut name);
            if checked == 0 {
                println!("{} ({:?}) no checks performed", name, violations.players[ply].lstick_type);
            } else if found == 0 {
                println!("{} ({:?}) passes", name, violations.players[ply].lstick_type);
            } else {
                println!("{} ({:?}) fails!", name, violations.players[ply].lstick_type);
                for violation in iter_violations(found) {
                    println!("  {}", violation_name(violation));
                }
            }
        }
    }
    
    std::process::ExitCode::SUCCESS
}

fn print_usage() {
    print!(
"USAGE:
    slp_enforcer <path/to/file>
"
    )
}
