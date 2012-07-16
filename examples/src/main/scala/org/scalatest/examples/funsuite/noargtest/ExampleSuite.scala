package org.scalatest.examples.funsuite.noargtest

import java.io.File
import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

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

  test("this test should succeed") {
    assert(1 + 1 === 2)
  }

  test("this test should fail") {
    assert(1 + 1 === 3)
  }
}