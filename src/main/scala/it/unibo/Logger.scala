package it.unibo

import org.slf4j.{Logger, LoggerFactory}

object Logger {
  def apply(): Logger = LoggerFactory.getLogger("")
}
