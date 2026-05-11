// Read every .parquet file in a directory using the Apache
// arrow-rs / parquet-rs stack, then print one OK / FAIL line
// per file. Used by the wireform-parquet interop driver to
// verify that wireform's Parquet output is consumable by the
// Rust ecosystem.

use std::env;
use std::fs::{self, File};
use std::process::ExitCode;

use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: read_parquet <dir>");
        return ExitCode::from(2);
    }

    let dir = &args[1];
    let mut total = 0;
    let mut failures = 0;

    let mut entries: Vec<_> = fs::read_dir(dir)
        .expect("dir")
        .filter_map(|e| e.ok())
        .collect();
    entries.sort_by_key(|e| e.file_name());

    for entry in entries {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("parquet") {
            continue;
        }
        total += 1;
        let name = path.file_name().unwrap().to_string_lossy().into_owned();

        let result = (|| -> Result<(usize, usize), Box<dyn std::error::Error>> {
            let file = File::open(&path)?;
            let builder = ParquetRecordBatchReaderBuilder::try_new(file)?;
            let schema = builder.schema().clone();
            let n_cols = schema.fields().len();
            let reader = builder.build()?;
            let mut n_rows = 0usize;
            for batch in reader {
                let batch = batch?;
                n_rows += batch.num_rows();
            }
            Ok((n_rows, n_cols))
        })();

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
    println!("{} files, {} ok, {} failed",
             total, total - failures, failures);

    if failures == 0 { ExitCode::SUCCESS } else { ExitCode::from((failures.min(99)) as u8) }
}
