package it.unibo

import org.yaml.snakeyaml.Yaml

import java.io.FileInputStream

object MultiLearningRunner extends App {
  val yaml = new Yaml()
  val loader = new FileInputStream("src/main/yaml/swapSourceGradient.yml")
  val result =
    yaml.load[java.util.Map[String, Object]](loader)

}
