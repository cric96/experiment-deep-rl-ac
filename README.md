# Towards Reinforcement Learning-based Aggregate Computing
## Case study: Hysteretic Q-Learning applied in Gradient Building blocks

This repository contains the experiments for *Towards Reinforcement Learning-based Aggregate Computing*.
These experiments serve as a motivation in which *learning* could be used to improve current *aggregate computing* blocks leaving the same high-level API.

### Requirements
- gradle;
- python > 3;
- ammonite shell 2.5.1 (for scala 2.13).
### Description

### Commands

`./gradlew runSwapSourceGradientRectangleLongBatch`

### Plots

`./src/main/plot/plotter.py src/main/plot/swapSourceSampling.yml data/ gradient.* myplot`

## Experiment

- YAML: `src/main/yaml/swapSourceGradientRectangleLong`
    - `episode` var ranges over [0..N..N+K] episodes where last K episodes are greedy
- Program: `it.unibo.scafi.casestudy.SwapSourceGradient`
- `class SwapSourceGradient extends HopCountLearningAlgorithms with SwapSourceLike with TemporalGradientRL`
- Running: `./gradlew runSwapSourceGradientRectangleLongBatch`
- Plotting: `./src/main/plot/plotter.py src/main/plot/swapSourceSampling.yml data/ gradient.* myplot`

**TODO**
