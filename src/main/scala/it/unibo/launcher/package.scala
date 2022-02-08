package it.unibo

import java.{util => jutil}
package object launcher {
  @SuppressWarnings(Array("org.wartremover.warts.All")) //because we have to deal with java world
  implicit class Unsafe(a: Any) {
    def as[T]: T = a.asInstanceOf[T]
    def list: jutil.List[Any] = as[jutil.List[Any]]
    def dict: jutil.Map[AnyRef, Any] = as[jutil.Map[AnyRef, Any]]
    def head: Any = list.get(0)
  }
}
