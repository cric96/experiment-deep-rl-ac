# Towards Aggregate Computing With Machine Learning
## Case study: Hysteretic Q-Learning applied in Hop Count Problem.

This repository contains the experiments for *Towards Aggregate Computing With Machine Learning"

### Requirements

### Description

### Commands

`./gradlew runSwapSourceGradientRectangleLongBatch`

### Plots

`./src/main/plot/plotter.py src/main/plot/swapSourceSampling.yml data/ gradient.* myplot`

## Experiment

- YAML: `src/main/yaml/swapSourceGradientRectangleLong`
- Program: `it.unibo.scafi.casestudy.SwapSourceGradient`
- `class SwapSourceGradient extends HopCountLearningAlgorithms with SwapSourceLike with TemporalGradientRL`
- Running: `./gradlew runSwapSourceGradientRectangleLongBatch`
- Plotting: `./src/main/plot/plotter.py src/main/plot/swapSourceSampling.yml data/ gradient.* myplot`

**TODO**