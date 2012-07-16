package org.scalatest.examples.featurespec.noargtest

import java.io.File
import org.scalatest.FeatureSpec

class ExampleSpec extends FeatureSpec {

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

  feature("Calculator Add") {
    scenario("1 + 1 should be 2") {
      assert(1 + 1 === 2)
    }

    scenario("2 + 2 should be 4") {
      assert(2 + 2 === 3)
    }
  }
}