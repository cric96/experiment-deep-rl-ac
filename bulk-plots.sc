import $ivy.`com.lihaoyi::os-lib:0.7.8`

@main
def regexRange(name: String, start: Int, end: Int) = {
  (start to end).map(num =>
    os.proc("python", "src/main/plot/plotter.py", "src/main/plot/swapSource.yml", "data/", s""".*${num}.*""", name).call())
}

@main
def regexAll(name: String) = {
    os.proc("python", "src/main/plot/plotter.py", "src/main/plot/swapSource.yml", "data/", s""".*""", name).call()
}