package org.scalatest.examples.funsuite.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSuite extends fixture.FunSuite {

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

  test("testing should be easy") { f =>
    f.writer.write("easy!")
    f.writer.flush()
    assert(f.file.length === 18)
  }

  test("testing should be fun") { f =>
    f.writer.write("fun!")
    f.writer.flush()
    assert(f.file.length === 17)
  }
}