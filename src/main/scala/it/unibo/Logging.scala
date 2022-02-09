package it.unibo

import org.slf4j.{Logger, LoggerFactory}

object Logging {
  def apply(): Logger = LoggerFactory.getLogger("")
}
