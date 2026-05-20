# Client Examples

Five runnable demos covering the common paths.

## Run them

```bash
cabal run wireform-kafka-client-examples produce
cabal run wireform-kafka-client-examples produce-typed
cabal run wireform-kafka-client-examples consume
cabal run wireform-kafka-client-examples group
cabal run wireform-kafka-client-examples transaction
```

All expect a broker at `localhost:9092`.

## The examples

| Name | What it shows |
|---|---|
| `produce` | Simplest producer. `withProducer` and `sendMessage` |
| `produce-typed` | Using typed topics with bundled serializers |
| `consume` | Low-level consumer with manual poll/commit |
| `group` | High-level consumer with automatic group management |
| `transaction` | Atomic sends across partitions |

## Which to start with

**Sending:** `produce` first, then `produce-typed` to see type-safe topics.

**Receiving:** `group` for automatic management. Use `consume` only if you need control over polling.

**Exactly-once:** `transaction` shows the full lifecycle.

## From example to production

All examples use brackets (`withProducer`, `withConsumer`) for resource cleanup. Use this pattern in production too.

Typed topics (`Topic.Topic k v`) catch serialization mismatches at compile time.

## See also

- [Tutorial](../TUTORIAL.md) - Walkthrough with more context
- [Concepts](../CONCEPTS.md) - Kafka fundamentals
- [README](../README.md) - Full API reference
