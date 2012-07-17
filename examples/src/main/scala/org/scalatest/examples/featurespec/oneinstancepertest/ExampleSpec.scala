package org.scalatest.examples.featurespec.oneinstancepertest

import org.scalatest._
import collection.mutable.ListBuffer

class ExampleSuite extends FeatureSpec with OneInstancePerTest {

  val builder = new StringBuilder("ScalaTest is ")
  val buffer = new ListBuffer[String]

  feature("Fixtures can be shared") {
    scenario("User learns how to share fixtures") {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    scenario("User enjoys reading tests with shared fixture") {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
    } 
  }
}