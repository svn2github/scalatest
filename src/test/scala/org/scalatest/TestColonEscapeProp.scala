package org.scalatest

import org.scalatest.prop.Tables
import org.scalatest.junit.JUnit3Suite
import org.scalatest.junit.JUnitSuite
import org.scalatest.testng.TestNGSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.events.Ordinal
import org.scalatest.events.IndentedText
import org.junit.Test
import org.testng.annotations.{Test => TestNGTest}
import org.scalatest.events.Formatter

trait TestColonEscapeExamples extends Tables {
  def suite: Suite
  def fixtureSuite: fixture.Suite
    
  def examples =
  Table(
    ("suite", "expected"),
    (suite, "- A Test"), 
    (fixtureSuite, "- A Test"))
}
  
trait NonTestColonEscapeExamples extends Tables {
  def funSuite: FunSuite
  def fixtureFunSuite: fixture.FunSuite
  def funSpec: FunSpec
  def fixtureFunSpec: fixture.FunSpec
  def featureSpec: FeatureSpec
  def fixtureFeatureSpec: fixture.FeatureSpec
  def flatSpec: FlatSpec
  def fixtureFlatSpec: fixture.FlatSpec
  def freeSpec: FreeSpec
  def fixtureFreeSpec: fixture.FreeSpec
  def propSpec: PropSpec
  def fixturePropSpec: fixture.PropSpec
  def wordSpec: WordSpec
  def fixtureWordSpec: fixture.WordSpec
  def pathFreeSpec: path.FreeSpec
  def pathFunSpec: path.FunSpec
  def junit3Suite: JUnit3Suite
  def junitSuite: JUnitSuite
  def testngSuite: TestNGSuite
    
  def examples =
  Table(
    ("suite", "expected"),
    (funSuite, "- test: A Test"),
    (fixtureFunSuite, "- test: A Test"),
    (funSpec, "- test: A Test"),
    (fixtureFunSpec, "- test: A Test"),
    (featureSpec, "Scenario: test: A Test"),
    (fixtureFeatureSpec, "Scenario: test: A Test"),
    (flatSpec, "- should test: A Test"),
    (fixtureFlatSpec, "- should test: A Test"),
    (freeSpec, "- test: A Test"),
    (fixtureFreeSpec, "- test: A Test"),
    (propSpec, "- test: A Test"),
    (fixturePropSpec, "- test: A Test"),
    (wordSpec, "- should test: A Test"),
    (fixtureWordSpec, "- should test: A Test"),
    (pathFreeSpec, "- test: A Test"),
    (pathFunSpec, "- test: A Test"),
    (junit3Suite, "- test: A Test(org.scalatest.TestColonEscapeExampleJUnit3Suite)"),
    (junitSuite, "- test: A Test"),
    (testngSuite, "- test: A Test"))
}

class TestColonEscapeProp extends FunSuite with TestColonEscapeExamples with TableDrivenPropertyChecks with ShouldMatchers with SharedHelpers {

  test("Suite and fixture.Suite should escape 'test:' prefix in its IndentedText's formattedText") {
    forAll(examples) { (suite, expected) =>
      val reporter = new EventRecordingReporter
      suite.run(None, RunArgs(reporter))
      assert(reporter.testSucceededEventsReceived.size === 1)
      val testSucceeded = reporter.testSucceededEventsReceived(0)
      testSucceeded.formatter match {
        case Some(formatter) =>
          formatter match {
            case IndentedText(formattedText, _, _) =>
              assert(formattedText === expected)
            case _ =>
              fail("Expected Some(IndentedText as formatter, but got: " + formatter)
          }
        case None =>
          fail("Expected Some(IndentedText) as formatter, but got None.")
      }
    }
  }
  
  def suite = new ExampleSuite()
  class ExampleSuite extends Suite {
    def `test: A Test`() {}
  }
  
  def fixtureSuite = new ExampleFixtureSuite
  class ExampleFixtureSuite extends fixture.Suite with StringFixture {
    def `test: A Test`() {}
  }
}

class TestColonEscapeExampleJUnit3Suite extends JUnit3Suite {
  def `test: A Test`() {}
}

class  TestColonEscapeExampleJUnitSuite extends JUnitSuite {
  @Test
  def `test: A Test`() {}
}

class TestColonEscapeExampleTestNGSuite extends TestNGSuite {
  @TestNGTest
  def `test: A Test`() {}
}

class NonTestColonEscapeProp extends FunSuite with NonTestColonEscapeExamples with TableDrivenPropertyChecks with ShouldMatchers with SharedHelpers {
  test("All others style trais besides Suite and fixture.Suite should not escape 'test:' prefix in its IndentedText's formattedText") {
    forAll(examples) { (suite, expected) =>
      val reporter = new EventRecordingReporter
      suite.run(None, RunArgs(reporter))
      assert(reporter.testSucceededEventsReceived.size === 1)
      val testSucceeded = reporter.testSucceededEventsReceived(0)
      testSucceeded.formatter match {
        case Some(formatter) =>
          formatter match {
            case IndentedText(formattedText, _, _) =>
              assert(formattedText === expected)
            case _ =>
              fail("Expected Some(IndentedText as formatter, but got: " + testSucceeded.formatter)
          }
        case None =>
          fail("Expected Some(IndentedText) as formatter, but got None.")
      }
    }
  }
  
  def funSuite = new ExampleFunSuite()
  class ExampleFunSuite extends FunSuite {
    test("test: A Test") {}
  }
  
  def fixtureFunSuite = new ExampleFixtureFunSuite
  class ExampleFixtureFunSuite extends fixture.FunSuite with StringFixture {
    test("test: A Test") { fixture => }
  }
  
  def funSpec = new ExampleFunSpec
  class ExampleFunSpec extends FunSpec {
    describe("A Spec") {
      it ("test: A Test") {}
    }
  }
  
  def fixtureFunSpec = new ExampleFixtureFunSpec
  class ExampleFixtureFunSpec extends fixture.FunSpec with StringFixture {
    describe("A Spec") {
      it ("test: A Test") { fixture => }
    }
  }
  
  def featureSpec = new ExampleFeatureSpec
  class ExampleFeatureSpec extends FeatureSpec {
    scenario("test: A Test") {}
  }
  
  def fixtureFeatureSpec = new ExampleFixtureFeatureSpec
  class ExampleFixtureFeatureSpec extends fixture.FeatureSpec with StringFixture {
    scenario("test: A Test") { fixture => }
  }
  
  def flatSpec = new ExampleFlatSpec
  class ExampleFlatSpec extends FlatSpec {
    "A Scope" should "test: A Test" in {}
  }
  
  def fixtureFlatSpec = new ExampleFixtureFlatSpec
  class ExampleFixtureFlatSpec extends fixture.FlatSpec with StringFixture {
    "A Scope" should "test: A Test" in { fixture => }
  }
  
  def freeSpec = new ExampleFreeSpec
  class ExampleFreeSpec extends FreeSpec {
    "A Scope" - {
      "test: A Test" in {}
    }
  }
  
  def fixtureFreeSpec = new ExampleFixtureFreeSpec
  class ExampleFixtureFreeSpec extends fixture.FreeSpec with StringFixture {
    "A Scope" - {
      "test: A Test" in { fixture => }
    }
  }
  
  def propSpec = new ExamplePropSpec
  class ExamplePropSpec extends PropSpec {
    property("test: A Test") {}
  }
  
  def fixturePropSpec = new ExampleFixturePropSpec
  class ExampleFixturePropSpec extends fixture.PropSpec with StringFixture {
    property("test: A Test") { fixture => }
  }
  
  def wordSpec = new ExampleWordSpec
  class ExampleWordSpec extends WordSpec {
    "A Scope" should {
      "test: A Test" in {}
    }
  }
  
  def fixtureWordSpec = new ExampleFixtureWordSpec
  class ExampleFixtureWordSpec extends fixture.WordSpec with StringFixture {
    "A Scope" should { 
      "test: A Test" in { fixture => }
    }
  }
  
  def pathFreeSpec = new ExamplePathFreeSpec
  class ExamplePathFreeSpec extends path.FreeSpec {
    "A Scope" - {
      "test: A Test" in {}
    }
  }
  
  def pathFunSpec = new ExamplePathFunSpec
  class ExamplePathFunSpec extends path.FunSpec {
    describe("A Spec") {
      it ("test: A Test") { }
    }
  }
  
  def junit3Suite = new TestColonEscapeExampleJUnit3Suite
  def junitSuite = new TestColonEscapeExampleJUnitSuite
  def testngSuite = new TestColonEscapeExampleTestNGSuite
}