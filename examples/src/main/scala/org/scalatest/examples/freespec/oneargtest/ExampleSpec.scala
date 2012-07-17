package org.scalatest.examples.freespec.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSpec extends fixture.FreeSpec {

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

  "Testing" - {
    "should be easy" in { f =>
      f.writer.write("easy!")
      f.writer.flush()
      assert(f.file.length === 18)
    }

    "should be fun" in { f =>
      f.writer.write("fun!")
      f.writer.flush()
      assert(f.file.length === 17)
    }
  } 
}