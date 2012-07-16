package org.scalatest.examples.flatspec.noargtest

import java.io.File
import org.scalatest.FlatSpec

class ExampleSpec extends FlatSpec {

  final val tmpDir = "tmpDir"

  override def withFixture(test: NoArgTest) {

    try {
      super.withFixture(test)
    }
    catch {
      case e: Exception =>
        val currDir = new File(".")
        val fileNames = currDir.list()
        info("Dir snapshot: " + fileNames.mkString(", "))
        throw e
    }
  }

  behavior of "this test"
    
  it should "succeed" in {
    assert(1 + 1 === 2)
  }

  it should "fail" in {
    assert(1 + 1 === 3)
  }
}