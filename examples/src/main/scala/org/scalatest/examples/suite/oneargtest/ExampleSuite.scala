package org.scalatest.examples.suite.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSuite extends fixture.Suite {

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

  def `test: testing should be easy` (f: F) {
    f.writer.write("easy!")
    f.writer.flush()
    assert(f.file.length === 12)
  }

  def `test: testing should be fun` (f: F) {
    f.writer.write("fun!")
    f.writer.flush()
    assert(f.file.length === 9)
  }
}
