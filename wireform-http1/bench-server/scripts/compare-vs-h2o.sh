#!/usr/bin/env bash
# Reproducible benchmark: wireform-http1 (precomputed + runserver paths)
# vs h2o vs nginx, all serving the same 14-byte "Hello, world!\n" body
# on the same VM through wrk.
#
# Prereqs (Debian / Ubuntu):
#   sudo apt-get install -y h2o nginx wrk
#   cabal build wireform-http1:wireform-http1-bench-server \
#               wireform-http1:wireform-http1-runserver-bench
#
# Usage:
#   ./compare-vs-h2o.sh
#
# Ports:
#   18080  wireform-http1 (precomputed via runServer + BodyPreEncoded)
#   18081  wireform-http1 (normal encoder runServer)
#   18082  h2o (file.dir / sendfile)
#   18083  nginx (return 200)
set -euo pipefail

H2O_CONF=$(mktemp /tmp/h2o-hello.XXXX.conf)
H2O_WWW=$(mktemp -d /tmp/h2o-www.XXXXXX)
NGINX_CONF=$(mktemp /tmp/nginx-hello.XXXX.conf)
NGINX_TMP=$(mktemp -d /tmp/nginx-tmp.XXXXXX)
PIPELINE_LUA=$(mktemp /tmp/pipeline.XXXX.lua)

printf 'Hello, world!\n' > "$H2O_WWW/index.html"

cat > "$H2O_CONF" <<EOF
pid-file: /tmp/wireform-bench-h2o.pid
error-log: /tmp/wireform-bench-h2o.err
num-threads: 2
listen: 18082
hosts:
  "default":
    paths:
      "/":
        file.dir: $H2O_WWW
EOF

cat > "$NGINX_CONF" <<EOF
worker_processes 2;
worker_rlimit_nofile 65536;
events { worker_connections 4096; multi_accept on; use epoll; }
error_log /tmp/wireform-bench-nginx.err warn;
pid /tmp/wireform-bench-nginx.pid;
daemon off;
http {
  client_body_temp_path $NGINX_TMP/cb;
  proxy_temp_path $NGINX_TMP/pt;
  fastcgi_temp_path $NGINX_TMP/fc;
  uwsgi_temp_path $NGINX_TMP/ut;
  scgi_temp_path $NGINX_TMP/st;
  access_log off;
  sendfile on;
  tcp_nopush on;
  tcp_nodelay on;
  keepalive_timeout 65;
  keepalive_requests 100000;
  server_tokens off;
  server {
    listen 18083 default_server backlog=4096 reuseport deferred;
    location / { return 200 "Hello, world!\n"; default_type text/plain; }
  }
}
EOF

cat > "$PIPELINE_LUA" <<'EOF'
init = function(args)
   reqs = {}
   for i=1,8 do reqs[i] = wrk.format("GET", "/") end
   req = table.concat(reqs)
end
request = function() return req end
EOF

PIDS=()
cleanup() {
  for p in "${PIDS[@]}"; do kill "$p" 2>/dev/null || true; done
  wait 2>/dev/null || true
  rm -f "$H2O_CONF" "$NGINX_CONF" "$PIPELINE_LUA"
  rm -rf "$H2O_WWW" "$NGINX_TMP"
}
trap cleanup EXIT

# Start servers
cabal run -v0 wireform-http1:wireform-http1-bench-server -- 18080 +RTS -N2 &
PIDS+=($!)
cabal run -v0 wireform-http1:wireform-http1-runserver-bench -- 18081 +RTS -N2 &
PIDS+=($!)
cabal run -v0 wireform-http1:wireform-http1-static-bench-server -- 18084 "$H2O_WWW/index.html" text/html +RTS -N2 &
PIDS+=($!)
h2o -c "$H2O_CONF" &
PIDS+=($!)
nginx -c "$NGINX_CONF" -p /tmp -g "" &
PIDS+=($!)

# Wait for them to come up
for p in 18080 18081 18082 18083 18084; do
  for i in 1 2 3 4 5 6 7 8 9 10; do
    if curl -sf --max-time 1 "http://127.0.0.1:$p/" >/dev/null; then break; fi
    sleep 1
  done
done

echo "=== wireform-http1 vs h2o vs nginx | $(uname -m) $(nproc) cores ==="
echo "  18080 wireform precomputed     (in-memory, runServer + BodyPreEncoded)"
echo "  18081 wireform runServer       (in-memory, runServer + BodyBytes)"
echo "  18084 wireform sendfile        (disk, runServer + BodyFile -> sendfile(2))"
echo "  18082 h2o      file.dir        (disk, sendfile internally)"
echo "  18083 nginx    return 200      (in-memory)"
echo

for workload in "-t2 -c50" "-t4 -c200" "-t2 -c50 -s $PIPELINE_LUA"; do
  echo "### $workload ###"
  printf "  %-9s %-9s %-9s %-9s %-9s %-9s\n" pre run sf h2o nginx
  for run in 1 2 3 4 5; do
    PRE=$(wrk $workload -d10s http://127.0.0.1:18080/ 2>&1 | grep 'Requests/sec' | awk '{print int($2)}')
    RUN=$(wrk $workload -d10s http://127.0.0.1:18081/ 2>&1 | grep 'Requests/sec' | awk '{print int($2)}')
    SF=$(wrk $workload -d10s http://127.0.0.1:18084/ 2>&1 | grep 'Requests/sec' | awk '{print int($2)}')
    H2O=$(wrk $workload -d10s http://127.0.0.1:18082/ 2>&1 | grep 'Requests/sec' | awk '{print int($2)}')
    NG=$(wrk $workload -d10s http://127.0.0.1:18083/ 2>&1 | grep 'Requests/sec' | awk '{print int($2)}')
    printf "  %-9d %-9d %-9d %-9d %-9d   (sf/h2o=%d%%)\n" \
      "$PRE" "$RUN" "$SF" "$H2O" "$NG" \
      "$((SF * 100 / H2O))"
  done
  echo
done
