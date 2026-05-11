// Read every .arrows / .arrow file in a directory using
// arrow-rs's IPC reader. Stream files (.arrows) go through
// StreamReader; file-format (.arrow) through FileReader.
//
// As of arrow-rs 58 (Feb 2026), ListView / LargeListView are
// supported by the IPC convert + reader (apache/arrow-rs#9006).
// Earlier versions panicked on the ListView type tag; we used to
// run each file under catch_unwind to survive that. Now that the
// pin is 58.x we let errors flow through naturally and treat any
// failure as a real failure.

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

        let result: Result<(usize, usize), Box<dyn std::error::Error>> = (|| {
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
        })();

        match result {
            Ok((rows, cols)) => {
                println!("  OK   {} ({} rows x {} col)", name, rows, cols);
            }
            Err(e) => {
                println!("  FAIL {}: {}", name, e.to_string());
                failures += 1;
            }
        }
    }

    println!("{}", "-".repeat(60));
    println!("{} files, {} ok, {} failed", total, total - failures, failures);

    if failures == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::from((failures.min(99)) as u8)
    }
}
