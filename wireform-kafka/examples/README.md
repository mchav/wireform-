# wireform-kafka client examples

Five self-contained runnable demos that cover the everyday
producer / consumer / transactional paths. Each demo is a
complete Haskell module that you can read end-to-end and adapt
to your own app; the README's hello-world snippets all come
from here.

| name | module | what it shows |
|---|---|---|
| `produce` | `Kafka.Client.Examples.Produce` | the smallest possible producer: `withProducer` + `sendMessage` |
| `produce-typed` | `Kafka.Client.Examples.ProduceTyped` | `publish` against a typed `Topic k v` |
| `consume` | `Kafka.Client.Examples.Consume` | low-level `withConsumer` + `poll` + `commitSync` |
| `group` | `Kafka.Client.Examples.Group` | the high-level `runConsumer` — one handler per record |
| `transaction` | `Kafka.Client.Examples.Transaction` | the five-step transactional-producer recipe |

## Run

```bash
cabal run wireform-kafka-client-examples produce
cabal run wireform-kafka-client-examples consume
# etc.
```

With no arguments the executable prints the index.

Every demo assumes a broker reachable at `localhost:9092` (the
docker-compose fixture in `test-integration/docker-compose.yml`
provides one).

## Picking the right layer

The five demos correspond to the four layers documented in the
top-level README's "pick the layer you need" table:

  - For send: `Produce` / `ProduceTyped`.
  - For receive: `Group` (recommended) or `Consume` (custom
    loop).
  - For atomic produce + offset commit: `Transaction`.
