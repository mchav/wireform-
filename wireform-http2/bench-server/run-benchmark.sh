#!/bin/bash
set -e

# HTTP/2 Server Throughput Benchmark
# Compares wireform-http2 against the http2 Hackage package using h2load

WIREFORM_PORT=8080
HTTP2_PORT=8081
REQUESTS=100000
CLIENTS=10
STREAMS=100
WARMUP_REQUESTS=10000

echo "============================================"
echo "HTTP/2 Server Throughput Benchmark"
echo "============================================"
echo ""
echo "Configuration:"
echo "  Requests: $REQUESTS"
echo "  Clients: $CLIENTS"
echo "  Max concurrent streams: $STREAMS"
echo "  Warmup: $WARMUP_REQUESTS requests"
echo ""

# Kill any existing servers
pkill -f wireform-http2-bench-server 2>/dev/null || true
pkill -f http2-bench-server 2>/dev/null || true
sleep 1

# Start wireform server
echo "Starting wireform-http2 server on port $WIREFORM_PORT..."
cabal run wireform-http2-bench-server -- $WIREFORM_PORT &
WIREFORM_PID=$!
sleep 2

# Start http2 server
echo "Starting http2 (Hackage) server on port $HTTP2_PORT..."
cabal run http2-bench-server -- $HTTP2_PORT &
HTTP2_PID=$!
sleep 2

echo ""
echo "============================================"
echo "Warmup: wireform-http2"
echo "============================================"
h2load -n $WARMUP_REQUESTS -c $CLIENTS -m $STREAMS http://127.0.0.1:$WIREFORM_PORT/ > /dev/null 2>&1 || true

echo ""
echo "============================================"
echo "Warmup: http2 (Hackage)"
echo "============================================"
h2load -n $WARMUP_REQUESTS -c $CLIENTS -m $STREAMS http://127.0.0.1:$HTTP2_PORT/ > /dev/null 2>&1 || true

echo ""
echo "============================================"
echo "Benchmark: wireform-http2"
echo "============================================"
echo ""
h2load -n $REQUESTS -c $CLIENTS -m $STREAMS http://127.0.0.1:$WIREFORM_PORT/

echo ""
echo "============================================"
echo "Benchmark: http2 (Hackage)"
echo "============================================"
echo ""
h2load -n $REQUESTS -c $CLIENTS -m $STREAMS http://127.0.0.1:$HTTP2_PORT/

echo ""
echo "============================================"
echo "Latency comparison (1 client, 1 stream)"
echo "============================================"
echo ""
echo "--- wireform-http2 ---"
h2load -n 10000 -c 1 -m 1 http://127.0.0.1:$WIREFORM_PORT/

echo ""
echo "--- http2 (Hackage) ---"
h2load -n 10000 -c 1 -m 1 http://127.0.0.1:$HTTP2_PORT/

# Cleanup
kill $WIREFORM_PID 2>/dev/null || true
kill $HTTP2_PID 2>/dev/null || true
wait 2>/dev/null

echo ""
echo "Benchmark complete."
