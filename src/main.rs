use std::process::ExitCode;
use std::{collections::HashSet, env::args, path::Path};

use regex::Regex;
mod unfeature;
use unfeature::unfeature;

fn main() -> ExitCode {
    let usage = r"Usage: <input> <output> <match_features> <enabled_features>

    Remove all known features that are not enabled.

    <input>: Path to the input file
    <output>: Path to the output file
    <match_features>: Regex which matches all features that should be visited
    <enabled_features>: Comma-separated list of enabled features";
    let args = args().collect::<Vec<String>>();
    if args.len() != 5 {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    }
    let input = Path::new(&args[1]);
    if !input.exists() {
        eprintln!("Input file does not exist: {}", input.display());
        return ExitCode::FAILURE;
    }
    let output = Path::new(&args[2]);
    let Ok(known_features) = Regex::new(&args[3]) else {
        eprintln!("Invalid regex: {}", &args[3]);
        return ExitCode::FAILURE;
    };
    let enabled_features = args[4]
        .split(',')
        .map(ToString::to_string)
        .collect::<HashSet<String>>();

    let input = std::fs::read_to_string(input).unwrap();

    let code = unfeature(&input, known_features, enabled_features).expect("Failed to parse input");
    std::fs::write(output, code).expect("Failed to write output");
    ExitCode::SUCCESS
}
