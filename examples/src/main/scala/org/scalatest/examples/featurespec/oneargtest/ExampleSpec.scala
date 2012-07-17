package org.scalatest.examples.featurespec.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSpec extends fixture.FeatureSpec {

  case class F(file: File, writer: FileWriter)
  type FixtureParam = F

  def withFixture(test: OneArgTest) {
    val file = File.createTempFile("hello", "world") // create the fixture
    val writer = new FileWriter(file)
    try {
      writer.write("ScalaTest is ") // set up the fixture
      super.withFixture(test.toNoArgTest(F(file, writer))) // "loan" the fixture to the test
    }
    finally {
      writer.close() // clean up the fixture
    }
  }

  feature("Testing") {
    scenario("user can write test code easily") { f =>
      f.writer.write("easy!")
      f.writer.flush()
      assert(f.file.length === 18)
    }

    scenario("user's test code should be fun to read") { f =>
      f.writer.write("fun!")
      f.writer.flush()
      assert(f.file.length === 17)
    }
  } 
}