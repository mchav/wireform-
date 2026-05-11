# TLS test fixtures

Self-signed certificates used by `Network.TlsHandshakeSpec`. Both
the server and the client cert have a 100-year validity window and
are not used outside the test suite.

| File         | Subject       | SAN             | Use                        |
|--------------|---------------|-----------------|----------------------------|
| `server.crt` | CN=localhost  | DNS:localhost, DNS:kafka.test | server identity for `Network.TLS` test broker |
| `server.key` | (RSA-2048)    |                 | matching server private key |
| `client.crt` | CN=test-client|                 | client identity for mTLS handshake test |
| `client.key` | (RSA-2048)    |                 | matching client private key |

Regenerate (in this directory):

```
openssl req -x509 -newkey rsa:2048 -keyout server.key -out server.crt \
  -sha256 -days 36500 -nodes -subj "/CN=localhost" \
  -addext "subjectAltName=DNS:localhost,DNS:kafka.test"

openssl req -x509 -newkey rsa:2048 -keyout client.key -out client.crt \
  -sha256 -days 36500 -nodes -subj "/CN=test-client"
```
