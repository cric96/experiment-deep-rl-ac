import $ivy.`com.lihaoyi::os-lib:0.7.8`

@main
def main(name: String, nums: String*) = {
  nums.map(num =>
    os.proc("python", "src/main/plot/plotter.py", "src/main/plot/swapSource.yml", "data/", s""".*${num}.*""", name).call())
}