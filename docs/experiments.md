# CerviQ Training and Evaluation

This document records CerviQ's training and evaluation methodology together with
results from the final benchmark runs. The command-line tools use the normal game
engine and the original two-worm scenario states. They are intentionally separate
from the adaptive multi-worm food and roster behavior of the Gloss application.

The tools use process-global randomness and have no seed option. Commands with the
same arguments repeat the methodology, but not necessarily the exact episode
sequence or numerical result.

The quantitative tables below were derived from 41 aggregate reports retained
locally under `outputs/evaluation/` when this document was prepared. The
repository ignores `outputs/`, so those raw reports are not distributed by Git.
The tables document the recorded runs, but a clean clone cannot independently
audit their source values unless the reports are archived separately.

## Build

From the repository root:

```text
stack build
```

Run experiment commands from the repository root because registered agents and
direct model references use repository-relative paths.

## Final V4 training recipe

The dedicated command preserves the source-level final V4 configuration:

```text
stack run cerviq-train-v4
```

`app/TrainV4.hs` fixes 30,000 episodes, a 500-tick limit, alpha 0.1, gamma 0.9,
epsilon 1.0 down to 0.05 with decay 0.99985, and progress every 200 episodes.
It writes:

```text
data/models/qlearning_v4_reformed_diverse_30k.txt
data/models/qlearning_v4_reformed_diverse_30k_stats.txt
```

The final recipe starts from an empty Q-table. It is kept unchanged as the
historical recipe used to produce the final V4 model.

### Training environment

The final diverse training environment samples from the following distributions:

| Dimension | Distribution |
| --- | --- |
| Scenario | Arena 40%, Cave 30%, Corridors 30% |
| Opponent | Safe Greedy Food 40%, Safe Hunter 30%, Safe Random 20%, Random 10% |
| Starting side | Original 50%, first two bodies/directions swapped 50% |
| Food | Authored food cleared, then one ordinary random item |

Only each scenario's two default worms are used. Large Cave and the additional
interactive multi-worm starts are not part of this training distribution.

## Configurable training

`cerviq-train` can train any implemented state version:

```text
stack run cerviq-train -- VERSION [OPTIONS]
```

`VERSION` is `v1`, `v2`, `v3`, or `v4`. The exact supported options can be shown
with:

```text
stack run cerviq-train -- --help
```

The configurable trainer is primarily intended for shorter experiments and
configurable demonstrations. The dedicated `cerviq-train-v4` command remains the
record of the final 30,000-episode V4 recipe.

## Pairwise evaluation

A registered agent can be evaluated against another registered agent with:

```text
stack run cerviq-evaluate -- \
  --agent "Q-learning V4 Reformed 30k + fallback" \
  --opponent "Safe Greedy Food" \
  --scenario Arena \
  --episodes 1000 \
  --max-ticks 500
```

Useful discovery commands are:

```text
stack run cerviq-evaluate -- --list-agents
stack run cerviq-evaluate -- --list-scenarios
```

The evaluator also accepts direct Q-table references using the prefixes `v1:`,
`v2:`, `v3:`, `v3f:`, `v4:`, and `v4f:`. The `f` variants use the corresponding
fallback policy.

### Evaluation methodology

Every benchmark in the local main evaluation set uses the same setup:

1. construct `Scenario.scenarioInitialState`, containing the two default worms;
2. randomly keep or swap the two starting bodies and directions;
3. clear authored food and place exactly one ordinary random food item;
4. assign the selected controllers to Worms 1 and 2;
5. simulate for at most 500 ticks;
6. stop earlier only if all worms are dead.

All main benchmark reports and all direct learned-agent comparisons use **1,000
episodes per matchup and scenario**. Final-model self-play is stored as two
independent 500-episode shards per scenario and combined as a 1,000-episode
symmetry check. The start side and food location are re-randomized for every
episode.

The main performance measure in the tables below is **last-standing rate**. This
is not called a win rate: a worm counts as last-standing if it becomes the only
living worm at any point, even if it later dies. Final survival rate is therefore
reported separately where useful.

When a single mean is shown across Arena, Cave, and Corridors, it is an
**unweighted macro-average across the three scenarios**. Large Cave is kept
separate because it was not part of the training distribution.

The local reports contain aggregate statistics rather than per-episode traces.
The comparisons below are therefore descriptive; no significance claim is made
for small differences between stochastic runs.

# Evaluation results

## 1. Learned-policy progression

The primary progression experiment compares successive **raw learned policies
without fallback** against the same `Safe Greedy Food` reference opponent. This
keeps the opponent and evaluation protocol fixed while the learned representation
and model change.

| Model | Arena LS | Cave LS | Corridors LS | Mean LS | Mean survival | Mean age | Food / 100 ticks |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| V1 | 48.2% | 47.4% | 28.9% | 41.5% | 24.5% | 229.4 | 3.39 |
| V2 | 70.5% | 76.1% | 50.4% | 65.7% | 61.7% | 392.9 | 3.49 |
| V3 Diverse 30k | 73.0% | 79.5% | 48.2% | 66.9% | 49.6% | 377.7 | 3.53 |
| V4 Diverse 30k | 76.0% | 85.1% | 69.0% | 76.7% | 40.4% | 366.5 | 4.03 |
| V4 Reformed 30k | 78.0% | 87.1% | 71.6% | 78.9% | 51.3% | 387.1 | 4.33 |

### Interpretation

The largest early improvement is from V1 to V2: mean last-standing rises from
41.5% to 65.7%, with substantial gains on all three maps. V2 also survives much
longer on average.

V3 Diverse is a more mixed step. It improves last-standing on Arena and Cave but
falls from 50.4% to 48.2% on Corridors, producing only a small macro-average gain
over V2. Its final survival rate is also lower than V2's. This is useful evidence
that later state representations did not improve every metric monotonically.

V4 Diverse produces a clearer competitive improvement, especially on Corridors,
where last-standing rises from 48.2% for V3 to 69.0%. Its mean last-standing rate
reaches 76.7%. At the same time its final survival rate is lower than V3's. The
combination demonstrates why last-standing and final survival should be treated
as different metrics: a policy can more often become the sole survivor without
necessarily remaining alive until the tick limit.

V4 Reformed gives a small but consistent last-standing improvement over V4
Diverse on all three training maps (78.0%, 87.1%, and 71.6%). It also recovers
some of the lost survival and age while achieving the highest raw-policy food
rate. The 2.2 percentage-point macro last-standing increase over V4 Diverse is
small enough that it should be treated as descriptive rather than as a strong
statistical claim by itself.

## 2. Deterministic fallback ablation

V3 and V4 can combine their learned Q-table with a deterministic safety/topology
fallback. To isolate its practical effect, the same learned table is evaluated
with and without fallback against `Safe Greedy Food`.

### Last-standing by scenario

| Model | Arena raw → fallback | Cave raw → fallback | Corridors raw → fallback | Mean change |
| --- | ---: | ---: | ---: | ---: |
| V3 Diverse 30k | 73.0% → 81.1% | 79.5% → 80.0% | 48.2% → 49.0% | +3.1 pp |
| V4 Diverse 30k | 76.0% → 91.7% | 85.1% → 93.1% | 69.0% → 81.5% | +12.1 pp |
| V4 Reformed 30k | 78.0% → 88.6% | 87.1% → 92.2% | 71.6% → 82.6% | +8.9 pp |

### Macro-average supporting metrics

| Model | Raw LS | + fallback LS | Raw survival | + fallback survival | Raw age | + fallback age | Raw food/100 | + fallback food/100 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| V3 Diverse 30k | 66.9% | 70.0% | 49.6% | 58.0% | 377.7 | 401.8 | 3.53 | 3.45 |
| V4 Diverse 30k | 76.7% | 88.8% | 40.4% | 89.3% | 366.5 | 462.3 | 4.03 | 3.68 |
| V4 Reformed 30k | 78.9% | 87.8% | 51.3% | 88.1% | 387.1 | 456.3 | 4.33 | 3.67 |

The fallback has only a modest mean last-standing effect on V3, but a much larger
effect on both V4 tables. For V4 Diverse it adds 12.1 percentage points of mean
last-standing and almost 49 percentage points of final survival. For V4 Reformed
it adds 8.9 points of mean last-standing and 36.8 points of survival.

The safety gain is not free in every metric. Mean food per 100 ticks decreases
for both V4 tables when fallback is enabled. The final runtime policy should
therefore be described as a combination of learned Q-values and deterministic
safety/topology handling, rather than attributing all of its benchmark strength
to the learned table alone.

One non-monotonic result should be kept visible: under this particular
`Safe Greedy Food` benchmark, **V4 Diverse + fallback has the highest mean
last-standing rate (88.8%)**, slightly above V4 Reformed + fallback (87.8%). The
raw V4 Reformed policy is stronger than raw V4 Diverse in these runs, but adding
the fallback does not preserve that ordering exactly.

## 3. Final policy against heuristic opponents

The selected final interactive policy, `Q-learning V4 Reformed 30k + fallback`,
was evaluated against both heuristic opponents that are most useful as strong
references.

| Opponent | Arena LS | Cave LS | Corridors LS | Mean LS | Mean survival | Mean age | Food / 100 ticks |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Safe Greedy Food | 88.6% | 92.2% | 82.6% | 87.8% | 88.1% | 456.3 | 3.67 |
| Safe Hunter | 60.4% | 86.0% | 61.6% | 69.3% | 69.3% | 370.6 | 3.83 |

`Safe Hunter` is clearly the harder matchup in Arena and Corridors. Against it,
mean last-standing falls by about 18.5 percentage points relative to Safe Greedy,
and final survival falls by about 18.8 points. Cave remains the strongest map for
the final policy, with an 86.0% last-standing rate even against Safe Hunter.

These heuristic comparisons are the primary reference benchmark because every
learned model can be compared against the same fixed opponent policy. Direct
Q-learning-versus-Q-learning comparisons are treated as supplementary evidence
below.

## 4. Held-out Large Cave evaluation

Large Cave is excluded from the mixed training scenario distribution, so it is
reported separately as a held-out environment. The final policy was evaluated
against both heuristic references.

| Opponent | Agent LS | Agent survival | Opponent LS | Opponent survival | Agent avg age | Agent food/100 | Avg episode length |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Safe Greedy Food | 31.8% | 91.5% | 1.7% | 61.3% | 480.5 | 1.83 | 482.5 |
| Safe Hunter | 72.2% | 74.4% | 1.1% | 3.3% | 408.6 | 2.30 | 412.0 |

The 31.8% last-standing result against Safe Greedy should **not** be read as only
31.8% survival. The final policy survives to the end in 91.5% of Large Cave
episodes, while Safe Greedy also survives in 61.3%. The average episode lasts
482.5 of the allowed 500 ticks. On this much larger map, many episodes therefore
finish at the tick limit without either worm ever becoming the sole survivor.
This makes last-standing less directly comparable with the smaller training maps.

Against Safe Hunter, the final policy reaches 72.2% last-standing and 74.4%
final survival while the opponent survives only 3.3% of episodes. Together the
two Large Cave runs show that the final policy can operate successfully on the
held-out geometry, but they do not establish improvement over older learned
models because those models were not evaluated on Large Cave in this benchmark
set.

## 5. Supplementary direct learned-agent evaluation

The primary progression deliberately uses fixed heuristic opponents. Direct
learned-agent tests complement that benchmark by placing two learned policies in
the same episodes. These comparisons remain supplementary because they do not
provide the common fixed reference used in Sections 1--4.

### V4 Reformed vs V3 Diverse

`Q-learning V4 Reformed 30k + fallback` was evaluated directly against
`Q-learning V3 Diverse 30k + fallback` on Arena, Cave, and Corridors. Each map
was evaluated twice for 1,000 episodes: once with V4 Reformed assigned to Worm 1
and once with the controller assignments reversed. Physical starting sides were
still randomized independently every episode.

| Scenario | V4 LS as W1 | V4 LS as W2 | V3 LS as W1 | V3 LS as W2 | V4 seat-avg LS | V3 seat-avg LS | Difference |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Arena | 67.4% | 68.2% | 29.5% | 30.9% | 67.8% | 30.2% | +37.6 pp |
| Cave | 63.1% | 61.6% | 24.3% | 25.7% | 62.4% | 25.0% | +37.4 pp |
| Corridors | 37.3% | 35.6% | 24.7% | 27.3% | 36.5% | 26.0% | +10.5 pp |
| **Macro average** | **55.9%** | **55.1%** | **26.2%** | **28.0%** | **55.5%** | **27.1%** | **+28.5 pp** |

Supporting metrics averaged over both controller assignments show the same
overall pattern:

| Metric | V4 Reformed + fallback | V3 Diverse + fallback |
| --- | ---: | ---: |
| Macro last-standing | 55.5% | 27.1% |
| Macro survival | 71.3% | 34.6% |
| Mean age | 425.8 | 351.6 |
| Food / 100 ticks | 3.34 | 3.67 |

V4 Reformed therefore has a large direct competitive advantage over V3 Diverse
in Arena and Cave and a smaller but still positive advantage in Corridors. The
last-standing gap remains large in both controller assignments, so the conclusion
is not caused by choosing one fixed Worm-1/Worm-2 assignment. V4's own
last-standing rate changes by only 0.8 percentage points in Arena, 1.5 points in
Cave, and 1.7 points in Corridors when the assignments are reversed.

The food-rate result is deliberately kept visible: V3 has a slightly higher
macro food-per-100-ticks value, driven mainly by Arena, despite much lower
survival and last-standing. Food collection efficiency is therefore not a
substitute for competitive survival in this environment.

### Final-model self-play

The final policy was also evaluated against itself as a symmetry and sanity
check. Each scenario consists of two independent 500-episode shards. Because the
shards have equal size, the combined values below are their simple averages and
represent 1,000 episodes per scenario.

| Scenario | Worm 1 LS | Worm 2 LS | LS difference | Worm 1 survival | Worm 2 survival | Avg episode length |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Arena | 43.2% | 35.6% | +7.6 pp | 64.1% | 56.8% | 499.6 |
| Cave | 39.4% | 36.9% | +2.5 pp | 63.1% | 60.6% | 500.0 |
| Corridors | 40.4% | 36.5% | +3.9 pp | 63.5% | 59.6% | 500.0 |

The two identical controllers behave broadly similarly, but Worm 1 has a
consistent descriptive advantage in all three self-play scenarios. The largest
gap is 7.6 percentage points of last-standing in Arena. Because physical starting
sides are randomized, this cannot simply be attributed to always receiving one
particular map side. It may reflect a remaining worm-ID/update-order asymmetry or
stochastic sampling variation. The aggregate reports are not sufficient to
distinguish those explanations statistically, so this is recorded as a symmetry
diagnostic rather than a performance claim.

Self-play also illustrates the effect of the 500-tick limit. Average episode
length is essentially 500 ticks on every map, and the two last-standing rates do
not sum to 100%. Many episodes therefore end while both identical policies are
still alive.

Importantly, the modest self-play asymmetry does not explain the direct V4/V3
result: that experiment explicitly reverses the controller assignments, and V4
Reformed remains substantially ahead of V3 Diverse in both assignments.

## 6. Overall experimental conclusions

The benchmark results support the following descriptive conclusions:

- V2 provides the largest early improvement over V1, while V3 is a mixed step
  rather than a uniform improvement on every map and metric.
- V4 produces the clearest later competitive gain, particularly on Corridors.
  V4 Reformed improves the raw V4 policy modestly and consistently in the three
  main scenarios.
- The deterministic fallback is a substantial part of practical V4 performance.
  It strongly increases survival and last-standing, while sometimes reducing
  food collected per unit time. The final controller must therefore be described
  as a hybrid learned-plus-deterministic policy.
- The selected V4 Reformed + fallback policy performs strongly against Safe
  Greedy Food and remains competitive against the stronger Safe Hunter baseline.
- On held-out Large Cave, high survival together with frequent 500-tick timeouts
  shows why last-standing alone can understate successful long-lived behaviour on
  large maps.
- Direct learned-agent evaluation confirms a substantial V4 Reformed advantage
  over V3 Diverse even when controller assignments are reversed.
- Final-model self-play is broadly symmetric but exposes a modest consistent
  Worm-1 advantage that should be documented as a remaining diagnostic rather
  than hidden.

These conclusions are descriptive rather than claims of statistical
significance because the evaluator stores aggregate reports and uses unseeded
randomness.

## Local report corpus

The local evaluation corpus used to derive these tables contains **41 report
files**:

- 15 raw-policy progression reports: V1, V2, V3 Diverse, V4 Diverse, and V4
  Reformed against Safe Greedy Food on Arena, Cave, and Corridors;
- 9 fallback reports: V3 Diverse, V4 Diverse, and V4 Reformed with fallback
  against Safe Greedy Food on the same three maps;
- 3 final-policy reports against Safe Hunter on Arena, Cave, and Corridors;
- 2 held-out Large Cave reports, against Safe Greedy Food and Safe Hunter;
- 6 direct learned-agent reports comparing V4 Reformed + fallback with V3
  Diverse + fallback in both controller assignments on all three training maps;
- 6 final-model self-play shard reports, two 500-episode shards for each of
  Arena, Cave, and Corridors.

The first 35 files are 1,000-episode reports. The six self-play files are
500-episode shards that combine into three additional 1,000-episode logical
evaluations. Thus the corpus represents **38 logical 1,000-episode evaluations**
when each self-play shard pair is treated as one experiment.

## Interpretation limits

- Training and the main benchmark are one-versus-one even though interactive
  runtime code supports more opponents.
- The final V4 model was trained only in two-worm environments. Multi-opponent
  runtime support must not be described as multi-opponent Q-learning training.
- Large Cave is a held-out map and is not included in the three-map training-set
  macro averages.
- Randomness is unseeded, so repeating a command produces a new stochastic
  sample rather than the identical episode sequence.
- V3/V4 `+ fallback` policies include deterministic safety filters/rankings in
  addition to learned Q-values; they are not pure greedy Q-table policies.
- Last-standing and final survival measure different outcomes and should not be
  used interchangeably.
- The local benchmark reports contain aggregate statistics, so the current
  analysis is descriptive rather than a per-episode statistical significance
  study.
- Evaluation is simulation-based benchmarking, not automated unit testing. Unit
  tests are run separately through `stack test`.
