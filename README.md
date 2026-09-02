# CerviQ

CerviQ is a Haskell grid game in which continuously moving worms collect food,
grow, and compete on shared maps. It combines a Gloss desktop application,
simultaneous multi-worm collision resolution, heuristic controllers, four
generations of tabular Q-learning, and reusable training and evaluation code.

## Features

- **Play vs Agents:** control Worm 1 against one or more independently selected
  AI opponents.
- **Watch Agents:** configure and observe 2-9 AI worms, subject to the selected
  scenario's available starts.
- Pause, variable speed, Watch history stepping, worm focus, and Q-learning
  diagnostics.
- Four deterministic scenarios, including a 60 x 36 map rendered through a
  worm-following viewport.
- Five heuristic agents and persisted Q-learning V1-V4 models.
- Scenario- and roster-dependent interactive food, with a simple safeguard
  against spawning immediately beside living worm heads.
- Generic tabular training plus configurable training and pairwise evaluation
  commands using the normal game engine.
- A Tasty/HUnit suite covering deterministic core rules, multi-worm scenarios,
  adaptive food, Play outcomes, Q-learning updates, and evaluation bookkeeping.

> **Screenshot TODO — Multi-worm Watch Agents**
>
> Add `docs/images/watch-multi-worm.png`.
> Capture a running Large Cave Watch session with at least six differently colored worms, the two-column agent overview, and off-screen indicators visible.

## Quick start

Prerequisites:

- [Stack](https://docs.haskellstack.org/);
- a graphical desktop with OpenGL support for Gloss;
- the supplied files under `data/models/`.

From the repository root:

```text
stack build
stack test
stack run cerviQ
```

Run from the repository root because `AgentRegistry.loadAgentOptions` loads
model files by relative path. Interactive sessions start paused.

The dedicated final V4 training command is:

```text
stack run cerviq-train-v4
```

It performs a new 30,000-episode run and writes the configured model and raw
statistics files. It has no command-line options, resume support, or
checkpoints.

Short configurable experiments and pairwise benchmarks use:

```text
stack run cerviq-train -- --help
stack run cerviq-evaluate -- --help
```

See [Training and evaluation](docs/experiments.md) before running either tool.

## Application modes

**Watch Agents** supports a dynamic roster with one controller per worm. Its
minimum is two worms; its maximum is the smaller of nine and the selected
scenario's predefined start count.

**Play vs Agents** reserves the first predefined worm for the human and supports
one or more independently configured opponents. The maximum is the scenario
capacity minus the human start, capped at eight opponents.

| Scenario | Size | Watch capacity | Play opponent capacity |
| --- | ---: | ---: | ---: |
| Arena | 20 x 10 | 6 | 5 |
| Cave | 30 x 15 | 8 | 7 |
| Corridors | 30 x 15 | 6 | 5 |
| Large Cave | 60 x 36 | 9 | 8 |

## AI and Q-learning

The GUI registry includes Random, Safe Random, Greedy Food, Safe Greedy Food,
Safe Hunter, and multiple persisted V1-V4 Q-learning variants. The final V4
fallback controller is hybrid: it filters actions with deterministic safety,
topology, and loop-breaking rules before comparing learned Q-values.

The runtime analysis supports multiple living opponents. The supplied training
executable and final model path use the original two-worm scenarios, however,
so the included trained models were not separately trained for 6-9 worm games.

## Documentation

- [User documentation](docs/user-documentation.md): setup, menus, controls,
  modes, scenarios, agents, food, viewport behavior, and troubleshooting.
- [Developer documentation](docs/developer-documentation.md): data model,
  architecture, tick rules, multi-worm GUI design, Q-learning evolution, V4
  internals, training, evaluation, persistence, and extension points.
- [Training and evaluation](docs/experiments.md): configurable commands,
  output files, pairwise methodology, and benchmark interpretation.

## Repository overview

| Path | Responsibility |
| --- | --- |
| `src/` | Exposed library: game engine, scenarios, agents, GUI, training, and evaluation |
| `src/QLearning/` | Generic Q-learning plus V1-V4 designs and diagnostics |
| `app/` | GUI, final/configurable training, and pairwise evaluation entry points |
| `test/Spec.hs` | Tasty/HUnit automated unit-test suite |
| `data/models/` | Persisted text Q-tables; trainers can also create local statistics files |
| `outputs/evaluation/` | Ignored local destination for generated evaluation reports |
| `docs/` | User, developer, and experiment documentation |
| `cerviQ.cabal` | Library, executables, test target, dependencies, and package metadata |

## Current limitations

- Built-in maps and spawn positions are deterministic; there is no procedural
  map generator.
- Poison is supported by the engine but is absent from the built-in scenarios.
- Random agents and food spawning have no exposed seed control.
- Q-tables use derived `Show`/`Read` persistence without schema versioning.
- Raw reports behind the quantitative experiment summary are local ignored
  artifacts, not files distributed by the Git repository.

## License

CerviQ is distributed under the BSD 3-Clause License. See [LICENSE](LICENSE).
