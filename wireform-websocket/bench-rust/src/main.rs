//! End-to-end echo throughput benchmark using tungstenite-rs.
//!
//! Mirrors the Haskell criterion benchmark in
//! `../bench/Bench.hs`: one server thread, one client connection
//! on loopback, persistent across the measurement loop.  For each
//! payload size we measure the wall-clock time per round-trip
//! across a fixed iteration count.
//!
//! The numbers are directly comparable with the wireform / websockets
//! Haskell benches because:
//!
//!   * Same loopback transport (127.0.0.1 over TCP).
//!   * Same payload sizes (64 B, 1 KiB, 16 KiB, 128 KiB).
//!   * Same single-connection persistent-client shape (handshake
//!     amortised out of the measurement).
//!   * Tungstenite is the canonical pure-Rust WebSocket
//!     implementation; results approximate what an idiomatic
//!     blocking-Rust application would see.
//!
//! Build:  cargo build --release
//! Run:    ./target/release/tungstenite-bench
//!
//! Output one line per (size, mode) reporting mean µs per
//! round-trip, plus a final summary CSV for easy diffing against
//! the Haskell run.

use std::io::{Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::sync::mpsc;
use std::thread;
use std::time::Instant;

use tungstenite::{accept, client::IntoClientRequest, connect,
                  protocol::WebSocketConfig, stream::MaybeTlsStream,
                  Message, WebSocket};

const WARMUP_ITERS: usize = 100;
const MEASURE_ITERS: usize = 5_000;

fn main() {
    let sizes: [(&str, usize); 4] = [
        ("64B", 64),
        ("1KiB", 1024),
        ("16KiB", 16 * 1024),
        ("128KiB", 128 * 1024),
    ];

    println!("=== tungstenite-rs end-to-end echo bench ===");
    println!("warmup={}  measure={}  loopback={}",
             WARMUP_ITERS, MEASURE_ITERS, "127.0.0.1");
    println!();
    println!("{:>10}  {:>14}  {:>14}",
             "size", "text µs/RT", "binary µs/RT");
    println!("{}", "-".repeat(46));

    for (name, n) in sizes.iter() {
        let payload = vec![b'A'; *n];

        let text_us = bench_round_trip(name, &payload, true);
        let bin_us  = bench_round_trip(name, &payload, false);

        println!("{:>10}  {:>14.2}  {:>14.2}",
                 name, text_us, bin_us);
    }
}

fn bench_round_trip(name: &str, payload: &[u8], text: bool) -> f64 {
    let listener = TcpListener::bind("127.0.0.1:0")
        .expect("bind ephemeral port");
    let addr: SocketAddr = listener.local_addr()
        .expect("local addr");

    // Spawn an echo server.  Block-accepting one connection is
    // sufficient: the bench opens exactly one client.
    let server_thread = thread::spawn(move || {
        let (stream, _) = listener.accept().expect("accept");
        let mut ws: WebSocket<TcpStream> = accept(stream)
            .expect("server handshake");
        loop {
            match ws.read() {
                Ok(Message::Text(t)) => {
                    ws.write(Message::Text(t)).ok();
                    ws.flush().ok();
                }
                Ok(Message::Binary(bs)) => {
                    ws.write(Message::Binary(bs)).ok();
                    ws.flush().ok();
                }
                Ok(Message::Close(_)) => break,
                Ok(_)  => {}
                Err(_) => break,
            }
        }
    });

    // Connect.  We point at the listener's actual address; the
    // server thread is already inside accept().
    let url = format!("ws://{}:{}/", addr.ip(), addr.port());
    let req = url.into_client_request().expect("URI");
    let (mut client, _) = connect(req).expect("client handshake");

    // Disable Nagle on the underlying socket for low-latency
    // round-trips; matches what wireform-websocket does on accept
    // and connect.
    if let MaybeTlsStream::Plain(s) = client.get_ref() {
        let _ = s.set_nodelay(true);
    }

    // Warmup.
    let make_msg = |b: &[u8]| if text {
        Message::Text(String::from_utf8(b.to_vec()).unwrap())
    } else {
        Message::Binary(b.to_vec())
    };
    for _ in 0..WARMUP_ITERS {
        client.write(make_msg(payload)).expect("warmup send");
        client.flush().expect("warmup flush");
        client.read().expect("warmup recv");
    }

    // Measure.
    let start = Instant::now();
    for _ in 0..MEASURE_ITERS {
        client.write(make_msg(payload)).expect("send");
        client.flush().expect("flush");
        client.read().expect("recv");
    }
    let elapsed = start.elapsed();

    // Send close to unwind the server.
    let _ = client.close(None);
    let _ = client.flush();
    let _ = server_thread.join();

    let _ = name;
    elapsed.as_secs_f64() * 1_000_000.0 / MEASURE_ITERS as f64
}
