package org.scalatest.examples.featurespec.composingwithfixture

import org.scalatest._
import org.scalatest.AbstractSuite
import collection.mutable.ListBuffer

trait Builder extends AbstractSuite { this: Suite =>

  val builder = new StringBuilder

  abstract override def withFixture(test: NoArgTest) {
    builder.append("ScalaTest is ")
    try {
      super.withFixture(test) // To be stackable, must call super.withFixture
    }
    finally {
      builder.clear()
    }
  }
}

trait Buffer extends AbstractSuite { this: Suite =>

  val buffer = new ListBuffer[String]

  abstract override def withFixture(test: NoArgTest) {
    try {
      super.withFixture(test) // To be stackable, must call super.withFixture
    }
    finally {
      buffer.clear()
    }
  }
}

class ExampleSpec extends FeatureSpec with Builder with Buffer {

  feature("Testing") {
    scenario("User can write test code easily") {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
    }

    scenario("User's test code should be fun to read") {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
      buffer += "clear"
    }
  }
}