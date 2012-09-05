package org.scalatest

import org.scalatest.events.Event
import org.scalatest.time.Span
import org.scalatest.time.Millis
import org.scalatest.prop.Tables

trait TestTimeoutExpectedResults extends EventHelpers {
  def assertTestTimeoutTest(events: List[Event])
}

trait ParallelTestExecutionTestTimeoutExamples extends Tables {

  def testTimeoutSuite = new ExampleParallelTestExecutionTestTimeoutSuite()
  def testTimeoutFixtureSuite = new ExampleParallelTestExecutionTestTimeoutFixtureSuite()
  def testTimeoutSpec = new ExampleParallelTestExecutionTestTimeoutSpec()
  def testTimeoutFixtureSpec = new ExampleParallelTestExecutionTestTimeoutFixtureSpec()
  def testTimeoutFunSuite = new ExampleParallelTestExecutionTestTimeoutFunSuite()
  def testTimeoutFixtureFunSuite = new ExampleParallelTestExecutionTestTimeoutFixtureFunSuite()
  def testTimeoutFunSpec = new ExampleParallelTestExecutionTestTimeoutFunSpec()
  def testTimeoutFixtureFunSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFunSpec()
  def testTimeoutFeatureSpec = new ExampleParallelTestExecutionTestTimeoutFeatureSpec()
  def testTimeoutFixtureFeatureSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFeatureSpec()
  def testTimeoutFlatSpec = new ExampleParallelTestExecutionTestTimeoutFlatSpec()
  def testTimeoutFixtureFlatSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFlatSpec()
  def testTimeoutFreeSpec = new ExampleParallelTestExecutionTestTimeoutFreeSpec()
  def testTimeoutFixtureFreeSpec = new ExampleParallelTestExecutionTestTimeoutFixtureFreeSpec()
  def testTimeoutPropSpec = new ExampleParallelTestExecutionTestTimeoutPropSpec()
  def testTimeoutFixturePropSpec = new ExampleParallelTestExecutionTestTimeoutFixturePropSpec()
  def testTimeoutWordSpec = new ExampleParallelTestExecutionTestTimeoutWordSpec()
  def testTimeoutFixtureWordSpec = new ExampleParallelTestExecutionTestTimeoutFixtureWordSpec()
  
  def testTimeoutExamples =
    Table(
      "suite1",
      testTimeoutSuite, 
      testTimeoutFixtureSuite, 
      testTimeoutSpec, 
      testTimeoutFixtureSpec, 
      testTimeoutFunSuite, 
      testTimeoutFixtureFunSuite, 
      testTimeoutFunSpec, 
      testTimeoutFixtureFunSpec, 
      testTimeoutFeatureSpec, 
      testTimeoutFixtureFeatureSpec, 
      testTimeoutFlatSpec, 
      testTimeoutFixtureFlatSpec, 
      testTimeoutFreeSpec, 
      testTimeoutFixtureFreeSpec,
      testTimeoutPropSpec, 
      testTimeoutFixturePropSpec, 
      testTimeoutWordSpec, 
      testTimeoutFixtureWordSpec
    )
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutSuite extends Suite with TestTimeoutExpectedResults with ParallelTestExecution {
  def testMethod1() {}
  def testMethod2() { Thread.sleep(600) }
  def testMethod3() {}
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestStarting(events(3), "testMethod3")
    checkTestSucceeded(events(4), "testMethod3")
    // The missing one
    checkTestSucceeded(events(5), "testMethod2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureSuite extends fixture.Suite with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  def testMethod1() {}
  def testMethod2() { Thread.sleep(600) }
  def testMethod3() {}
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "testMethod1")
    checkTestSucceeded(events(1), "testMethod1")
    checkTestStarting(events(2), "testMethod2")
    checkTestStarting(events(3), "testMethod3")
    checkTestSucceeded(events(4), "testMethod3")
    // The missing one
    checkTestSucceeded(events(5), "testMethod2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutSpec extends Spec with TestTimeoutExpectedResults with ParallelTestExecution {
  def `test 1` {}
  def `test 2` { Thread.sleep(600) }
  def `test 3` {}
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestStarting(events(3), "test 3")
    checkTestSucceeded(events(4), "test 3")
    // The missing one
    checkTestSucceeded(events(5), "test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureSpec extends fixture.Spec with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  def `test 1`(fixture: String) {}
  def `test 2`(fixture: String) { Thread.sleep(600) }
  def `test 3`(fixture: String) {}
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "test 1")
    checkTestSucceeded(events(1), "test 1")
    checkTestStarting(events(2), "test 2")
    checkTestStarting(events(3), "test 3")
    checkTestSucceeded(events(4), "test 3")
    // The missing one
    checkTestSucceeded(events(5), "test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFunSuite extends FunSuite with TestTimeoutExpectedResults with ParallelTestExecution {
  test("Test 1") {}
  test("Test 2") { Thread.sleep(600) }
  test("Test 3") {}
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFunSuite extends fixture.FunSuite with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  test("Test 1") { fixture => }
  test("Test 2") { fixture => Thread.sleep(600) }
  test("Test 3") { fixture => }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFunSpec extends FunSpec with TestTimeoutExpectedResults with ParallelTestExecution {
  describe("Scope 1") {
    it("Test 1") {}
    it("Test 2") {}
  }
  describe("Scope 2") {
    it("Test 3") { Thread.sleep(600) }
    it("Test 4") {}
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFunSpec extends fixture.FunSpec with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  describe("Scope 1") {
    it("Test 1") { fixture => }
    it("Test 2") { fixture =>}
  }
  describe("Scope 2") {
    it("Test 3") { fixture => Thread.sleep(600) }
    it("Test 4") { fixture => }
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFeatureSpec extends FeatureSpec with TestTimeoutExpectedResults with ParallelTestExecution {
  feature("Scope 1") {
    scenario("Test 1") {}
    scenario("Test 2") {}
  }
  feature("Scope 2") {
    scenario("Test 3") { Thread.sleep(600) }
    scenario("Test 4") {}
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Feature: Scope 1")
    checkTestStarting(events(1), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestStarting(events(3), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(4), "Feature: Scope 1 Scenario: Test 2")
    checkScopeClosed(events(5), "Feature: Scope 1")
    checkScopeOpened(events(6), "Feature: Scope 2")
    checkTestStarting(events(7), "Feature: Scope 2 Scenario: Test 3")
    checkTestStarting(events(8), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(9), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(10), "Feature: Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Feature: Scope 2 Scenario: Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFeatureSpec extends fixture.FeatureSpec with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  feature("Scope 1") {
    scenario("Test 1") { fixture => }
    scenario("Test 2") { fixture =>}
  }
  feature("Scope 2") {
    scenario("Test 3") { fixture => Thread.sleep(600) }
    scenario("Test 4") { fixture => }
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Feature: Scope 1")
    checkTestStarting(events(1), "Feature: Scope 1 Scenario: Test 1")
    checkTestSucceeded(events(2), "Feature: Scope 1 Scenario: Test 1")
    checkTestStarting(events(3), "Feature: Scope 1 Scenario: Test 2")
    checkTestSucceeded(events(4), "Feature: Scope 1 Scenario: Test 2")
    checkScopeClosed(events(5), "Feature: Scope 1")
    checkScopeOpened(events(6), "Feature: Scope 2")
    checkTestStarting(events(7), "Feature: Scope 2 Scenario: Test 3")
    checkTestStarting(events(8), "Feature: Scope 2 Scenario: Test 4")
    checkTestSucceeded(events(9), "Feature: Scope 2 Scenario: Test 4")
    checkScopeClosed(events(10), "Feature: Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Feature: Scope 2 Scenario: Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFlatSpec extends FlatSpec with TestTimeoutExpectedResults with ParallelTestExecution {
  behavior of "Scope 1"
  it should "Test 1" in {}
  it should "Test 2" in {}
  
  behavior of "Scope 2"
  it should "Test 3" in { Thread.sleep(600) }
  it should "Test 4" in {}
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFlatSpec extends fixture.FlatSpec with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  behavior of "Scope 1"
  it should "Test 1" in { fixture => }
  it should "Test 2" in { fixture => }
  
  behavior of "Scope 2"
  it should "Test 3" in { fixture => Thread.sleep(600) }
  it should "Test 4" in { fixture => }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFreeSpec extends FreeSpec with TestTimeoutExpectedResults with ParallelTestExecution {
  "Scope 1" - {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" - {
    "Test 3" in { Thread.sleep(600) }
    "Test 4" in {}
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureFreeSpec extends fixture.FreeSpec with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  "Scope 1" - {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" - {
    "Test 3" in { fixture => Thread.sleep(600) }
    "Test 4" in { fixture => }
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 Test 1")
    checkTestSucceeded(events(2), "Scope 1 Test 1")
    checkTestStarting(events(3), "Scope 1 Test 2")
    checkTestSucceeded(events(4), "Scope 1 Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 Test 3")
    checkTestStarting(events(8), "Scope 2 Test 4")
    checkTestSucceeded(events(9), "Scope 2 Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutPropSpec extends PropSpec with TestTimeoutExpectedResults with ParallelTestExecution {
  property("Test 1") {}
  property("Test 2") { Thread.sleep(600) }
  property("Test 3") {}
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixturePropSpec extends fixture.PropSpec with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  property("Test 1") { fixture => }
  property("Test 2") { fixture => Thread.sleep(600) }
  property("Test 3") { fixture => }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 6)
    checkTestStarting(events(0), "Test 1")
    checkTestSucceeded(events(1), "Test 1")
    checkTestStarting(events(2), "Test 2")
    checkTestStarting(events(3), "Test 3")
    checkTestSucceeded(events(4), "Test 3")
    // The missing one
    checkTestSucceeded(events(5), "Test 2")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutWordSpec extends WordSpec with TestTimeoutExpectedResults with ParallelTestExecution {
  "Scope 1" should {
    "Test 1" in {}
    "Test 2" in {}
  }
  
  "Scope 2" should {
    "Test 3" in { Thread.sleep(600) }
    "Test 4" in {}
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}

@DoNotDiscover
class ExampleParallelTestExecutionTestTimeoutFixtureWordSpec extends fixture.WordSpec with TestTimeoutExpectedResults with ParallelTestExecution with StringFixture {
  "Scope 1" should {
    "Test 1" in { fixture => }
    "Test 2" in { fixture => }
  }
  
  "Scope 2" should {
    "Test 3" in { fixture => Thread.sleep(600) }
    "Test 4" in { fixture => }
  }
  
  override protected def sortingTimeout: Span = Span(300, Millis)
  
  def assertTestTimeoutTest(events: List[Event]) {
    assert(events.size === 12)
    checkScopeOpened(events(0), "Scope 1")
    checkTestStarting(events(1), "Scope 1 should Test 1")
    checkTestSucceeded(events(2), "Scope 1 should Test 1")
    checkTestStarting(events(3), "Scope 1 should Test 2")
    checkTestSucceeded(events(4), "Scope 1 should Test 2")
    checkScopeClosed(events(5), "Scope 1")
    checkScopeOpened(events(6), "Scope 2")
    checkTestStarting(events(7), "Scope 2 should Test 3")
    checkTestStarting(events(8), "Scope 2 should Test 4")
    checkTestSucceeded(events(9), "Scope 2 should Test 4")
    checkScopeClosed(events(10), "Scope 2")
    // The missing one
    checkTestSucceeded(events(11), "Scope 2 should Test 3")
  }
}
