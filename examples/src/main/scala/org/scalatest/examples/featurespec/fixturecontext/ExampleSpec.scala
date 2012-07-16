package org.scalatest.examples.featurespec.fixturecontext

import collection.mutable.ListBuffer
import org.scalatest.FeatureSpec

class ExampleSpec extends FeatureSpec {

  trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  trait Buffer {
    val buffer = ListBuffer("ScalaTest", "is")
  }

  feature("Fixtures can be shared") {
    // This test needs the StringBuilder fixture
    scenario("user should be productive when writes tests") {
      new Builder {
        builder.append("productive!")
        assert(builder.toString === "ScalaTest is productive!")
      }
    }
    
    // This test needs the ListBuffer[String] fixture
    scenario("user can write readable test code") {
      new Buffer {
        buffer += ("readable!")
        assert(buffer === List("ScalaTest", "is", "readable!"))
      }
    }

    // This test needs both the StringBuilder and ListBuffer
    scenario("user's test code should be clear and concise") {
      new Builder with Buffer {
        builder.append("clear!")
        buffer += ("concise!")
        assert(builder.toString === "ScalaTest is clear!")
        assert(buffer === List("ScalaTest", "is", "concise!"))
      }
    }
  }
}