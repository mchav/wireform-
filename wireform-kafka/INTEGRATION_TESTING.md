# Integration Testing

Running tests against a live Kafka broker.

## Getting a broker

### With Nix (easiest)

```bash
nix develop
start-kafka      # Kafka starts on localhost:9092
run-integration-tests
stop-kafka
```

### Without Nix

```bash
cd /tmp && curl -sL https://dlcdn.apache.org/kafka/4.0.0/kafka_2.13-4.0.0.tgz | tar xz
cd kafka_2.13-4.0.0

KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"
bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties
KAFKA_HEAP_OPTS="-Xmx1G -Xms512M" bin/kafka-server-start.sh config/kraft/server.properties
```

## Running tests

```bash
# All integration tests
cabal test wireform-kafka:wireform-kafka-integration

# Verbose
cabal test ... --test-arguments='--verbose'

# Pattern match
cabal test ... --test-arguments='--pattern "Connection"'
```

## Managing topics

With Nix:
```bash
create-test-topic my-topic          # Default: 3 partitions, RF 1
create-test-topic my-topic 5 1      # 5 partitions, RF 1
list-topics
stop-kafka
```

Direct Kafka CLI:
```bash
kafka-topics.sh --bootstrap-server localhost:9092 --describe --topic my-topic
kafka-console-producer.sh --bootstrap-server localhost:9092 --topic my-topic
kafka-console-consumer.sh --bootstrap-server localhost:9092 --topic my-topic --from-beginning
```

## Troubleshooting

| Issue | Fix |
|---|---|
| Port 9092 in use | Kill existing Kafka: `stop-kafka` |
| Tests slow | Check broker logs; run unit tests separately for faster feedback |
| Connection errors | Verify broker is listening: `nc -zv 127.0.0.1 9092` |

## CI setup

```bash
# Docker
docker run -d -p 9092:9092 apache/kafka:latest

# Then run tests
WIREFORM_KAFKA_BROKER=localhost:9092 cabal test wireform-kafka:wireform-kafka-integration
```

## Development workflow

```bash
nix develop
start-kafka

# Iterative testing
run-integration-tests

stop-kafka
```
