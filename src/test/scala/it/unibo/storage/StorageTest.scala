package it.unibo.storage

import org.junit.runner.RunWith
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.junit.JUnitRunner
import os.Path
import upickle.default.{macroRW, ReadWriter => RW}

import java.nio.file.NoSuchFileException

@RunWith(classOf[JUnitRunner])
class StorageTest extends AnyFlatSpec with should.Matchers with BeforeAndAfterEach {
  val storage = new LocalStorage[String]("folder")
  val wd: Path = os.pwd / "folder"
  case class Data(name: String)
  object Data {
    @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
    implicit val rw: RW[Data] = macroRW
  }
  case class ComplexData(key: String, map: Map[Int, String])
  object ComplexData {
    @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
    implicit val rw: RW[ComplexData] = macroRW
  }

  case class Generic[D](data: D)
  object Generic {
    @SuppressWarnings(Array("org.wartremover.warts.All")) // because of macro expansion
    implicit def rw[D: RW]: RW[Generic[D]] = macroRW
  }
  override def beforeEach(): Unit =
    storage.clean()
  override def afterEach(): Unit =
    storage.clean()

  "Local storage" should "write data" in {
    storage.save("mydata", Data("foo"))
    assert(os.exists(wd / "mydata"))
  }

  "Local storage" should "read data data" in {
    storage.save("mydata", Data("foo"))
    assert(storage.load[Data]("mydata") == Data("foo"))
  }

  "Local storage" should "write complex data" in {
    val map: Map[Int, String] = Map(1 -> "foo", 2 -> "scala")
    val complexData = ComplexData("complex", map)
    storage.save("mydata", complexData)
    assert(storage.load[ComplexData]("mydata") == complexData)
  }

  "Local storage" should "throw an exception if the file does not exist" in {
    assertThrows[NoSuchFileException] {
      storage.load[Data]("foo")
    }
  }

  "Clean" should "remove the files stored" in {
    storage.save("mydata", Data("foo"))
    assert(os.exists(wd / "mydata"))
    storage.clean()
    assert(!os.exists(wd / "mydata"))
  }

  "Local storage" should "load another object when a key does not exist" in {
    storage.loadOrElse("mydata", Data("foo")) shouldBe Data("foo")
  }

  "Local storage" should "load the stored object when it exists" in {
    val data = Data("foo")
    storage.save("mydata", data)
    storage.loadOrElse("mydata", Data("another")) shouldBe data
  }

  "Local storage" should "work with generic types" in {
    val generic = Generic(10)
    storage.save("generic", generic)
    succeed
  }
}
