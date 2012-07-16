package org.scalatest.examples.featurespec.beforeandafter

import org.scalatest.FeatureSpec
import org.scalatest.BeforeAndAfter
import collection.mutable.ListBuffer

class ExampleSpec extends FeatureSpec with BeforeAndAfter {

  val builder = new StringBuilder
  val buffer = new ListBuffer[String]

  before {
    builder.append("ScalaTest is ")
  }

  after {
    builder.clear()
    buffer.clear()
  }

  feature("Testing") {
    scenario("user can write test code easily") {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    scenario("user's test code should be fun to read") {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
    }
  }
}