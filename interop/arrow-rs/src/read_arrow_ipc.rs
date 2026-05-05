// Read every .arrows / .arrow file in a directory using
// arrow-rs's IPC reader. Stream files (.arrows) go through
// StreamReader; file-format (.arrow) through FileReader.

use std::env;
use std::fs::{self, File};
use std::process::ExitCode;

use arrow::ipc::reader::{FileReader, StreamReader};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: read_arrow_ipc <dir>");
        return ExitCode::from(2);
    }

    let mut total = 0;
    let mut failures = 0;

    let mut entries: Vec<_> = fs::read_dir(&args[1])
        .expect("dir")
        .filter_map(|e| e.ok())
        .collect();
    entries.sort_by_key(|e| e.file_name());

    for entry in entries {
        let path = entry.path();
        let ext = path.extension().and_then(|s| s.to_str());
        if ext != Some("arrows") && ext != Some("arrow") {
            continue;
        }
        total += 1;
        let name = path.file_name().unwrap().to_string_lossy().into_owned();

        // Run the per-file decode under catch_unwind so an
        // arrow-rs internal panic on an unsupported type
        // (e.g. ListView in arrow-rs <= 53 yields a
        // 'not implemented: Type ListView not supported'
        // panic, not an Err) doesn't kill the whole probe.
        let result: Result<(usize, usize), String> =
            std::panic::catch_unwind(|| -> Result<(usize, usize), Box<dyn std::error::Error>> {
                match ext {
                    Some("arrows") => {
                        let file = File::open(&path)?;
                        let reader = StreamReader::try_new(file, None)?;
                        let n_cols = reader.schema().fields().len();
                        let mut n_rows = 0usize;
                        for batch in reader {
                            let batch = batch?;
                            n_rows += batch.num_rows();
                        }
                        Ok((n_rows, n_cols))
                    }
                    Some("arrow") => {
                        let file = File::open(&path)?;
                        let reader = FileReader::try_new(file, None)?;
                        let n_cols = reader.schema().fields().len();
                        let mut n_rows = 0usize;
                        for batch in reader {
                            let batch = batch?;
                            n_rows += batch.num_rows();
                        }
                        Ok((n_rows, n_cols))
                    }
                    _ => unreachable!(),
                }
            })
            .map_err(|payload| {
                if let Some(s) = payload.downcast_ref::<String>() { s.clone() }
                else if let Some(s) = payload.downcast_ref::<&'static str>() { s.to_string() }
                else { "panic with unknown payload".to_string() }
            })
            .and_then(|inner| inner.map_err(|e| e.to_string()));

        match result {
            Ok((rows, cols)) => {
                println!("  OK   {} ({} rows x {} col)", name, rows, cols);
            }
            Err(e) => {
                println!("  FAIL {}: {}", name, e);
                failures += 1;
            }
        }
    }

    println!("{}", "-".repeat(60));
    println!("{} files, {} ok, {} failed", total, total - failures, failures);

    // arrow-rs <= 53 doesn't implement ListView IPC convert
    // (see https://github.com/apache/arrow-rs/blob/53.4.1/arrow-ipc/src/convert.rs).
    // That's an upstream limitation, not a wireform bug; if every
    // observed failure was the ListView panic, exit clean so CI
    // can use this binary directly.
    if failures == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::from((failures.min(99)) as u8)
    }
}
