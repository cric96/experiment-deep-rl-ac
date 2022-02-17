# Towards Reinforcement Learning-based Aggregate Computing
## Case study: Hysteretic Q-Learning applied in Gradient Building block

This repository contains the experiments for *Towards Reinforcement Learning-based Aggregate Computing*.
These experiments serve as a motivation in which *learning* could be used to improve current *aggregate computing* blocks leaving the same high-level API.

### Requirements
- gradle;
- python > 3;
- [ammonite shell](http://ammonite.io/) 2.5.1 (for scala 2.13).

### Project structure
  - [scala sources](src/main/scala): contain the sources used to define the aggregate program definition, the reinforcement learning implementation, a multi alchemist simulations launcher, and some utility to handle stochastic.
  - [plot script sources](src/main/scala): contain the source used to produce the plot presented in the paper. As scripting languages we use ammonite (a scala shell extension) and python.
  - [yaml simulation configuration](src/main/yaml): contain the declarative description of the simulation used to verify our reinforcement learning-based algorithm.

### Description
The aim of this repository is (i) to implement the first integration of aggregate computing and reinforcement learning applied at gradient block and (ii) to verify the behaviour through extensive use of simulation.

For the first point, we use [ScaFi](), an aggregate computing toolchain used to define collective behaviour. For the simulation part, we leverage [Alchemist]() a pervasive computing simulator.

The RL and AC integration is devised in different files.
In [GradientLikeLearning](src/main/scala/it/unibo/scafi/casestudy/GradientLikeLearning.scala) is defined the learning loop is used inside the aggregate program. In [TemporalGradientRL](src/main/scala/it/unibo/scafi/casestudy/algorithm/gradient/TemporalGradientRL.scala) we concrete the state, reward, action to be used in the gradient algorithm.
Finally, in [SwapSourceGradient](src/main/scala/it/unibo/scafi/casestudy/SwapSourceGradient.scala) we devise the aggregate program used to test our RL based implementation against CRF (i.e. one of the heuristics used to improve classical gradient implementation).

RL algorithms and abstraction are developed in the [it.unibo.learning](src/main/scala/it/unibo/learning) package.

### Commands

`./gradlew startMultipleLearning -PXfile="launch.txt"`

To reproduce the same data of the paper, use [launch-paper](launch-paper.txt).
The description of launch.txt could be found at [MultiLearningRunner](src/main/scala/it/unibo/launcher/MultiLearningRunner.scala).
The data are stored in data/
### Plots

`amm src/main/plot/process-simulations.sc --skip 1000`

This will produce the plots showed in the article. The images are stored in result/.

## Experiment summary
- YAML: `src/main/yaml/swapSourceGradientMultiEnvironment`
    - `episode` var ranges over [0..N..N+K] episodes where last K episodes are greedy
    - each 6 episode, the environment change from a line to a grid of nodes (40 nodes vs 200)
- Program: [`it.unibo.scafi.casestudy.SwapSourceGradient`](src/main/scala/it/unibo/scafi/casestudy/SwapSourceGradient.scala)
- Running: `./gradlew startMultipleLearning -PXfile="launch.txt"`
- Plotting: `./src/main/plot/plotter.py src/main/plot/swapSourceSampling.yml data/ gradient.* myplot`
