import $ivy.`com.lihaoyi::os-lib:0.7.8`

@main def plotInRange(output: String, start: Int, end: Int) = {
  (start to end).map(num =>
    os.proc("python", "src/main/plot/plotter.py", "src/main/plot/swapSource.yml", "data/", s""".*-${num}\\.0.*""", output).call())
}

@main def plotAll(output: String) = {
  os.proc("python", "src/main/plot/plotter.py", "src/main/plot/swapSource.yml", "data/", s""".*""", output).call()
}

@main def plotOnly(output: String, baseExperiment: String, configuration: String) = {
  os.proc("python", "src/main/plot/plotter.py", configuration, "data/", s""".*${baseExperiment}.*""", output).call()
}