package org.scalatest.events

import org.scalatest._

class LocationMethodSuiteProp extends MethodSuiteProp {
  
  test("Method suites should have correct TopOfMethod location in test events.") {
    forAll(examples) { suite =>
      val reporter = new EventRecordingReporter
      suite.run(None, Args(reporter, new Stopper {}, Filter(), Map(), None, new Tracker(new Ordinal(99)), Set.empty))
      val eventList = reporter.eventsReceived
      eventList.foreach { event => suite.checkFun(event) }
      suite.allChecked
    }
  }
  
  type FixtureServices = TestLocationMethodServices
  
  def suite = new TestLocationSuite
  class TestLocationSuite extends Suite with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationSuite"
    val expectedStartingList = List(TestStartingPair("testSucceed", "testSucceed"), 
                                TestStartingPair("testPending", "testPending"), 
                                TestStartingPair("testCancel", "testCancel"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "testSucceed"), 
                              TestResultPair(classOf[TestPending], "testPending"),
                              TestResultPair(classOf[TestCanceled], "testCancel"),
                              TestResultPair(classOf[TestIgnored], "testIgnore"))
    
    def testSucceed() {
      
    }
    def testPending() {
      pending
    }
    def testCancel() {
      cancel
    }
    @Ignore
    def testIgnore() {
      
    }
  }
  
  def fixtureSuite = new TestLocationFixtureSuite
  class TestLocationFixtureSuite extends fixture.Suite with FixtureServices with StringFixture {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationFixtureSuite"
    val expectedStartingList = List(TestStartingPair("testSucceed", "testSucceed"), 
                                TestStartingPair("testPending", "testPending"), 
                                TestStartingPair("testCancel", "testCancel"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "testSucceed"), 
                              TestResultPair(classOf[TestPending], "testPending"),
                              TestResultPair(classOf[TestCanceled], "testCancel"),
                              TestResultPair(classOf[TestIgnored], "testIgnore"))
    
    def testSucceed() {
      
    }
    def testPending() {
      pending
    }
    def testCancel() {
      cancel
    }
    @Ignore
    def testIgnore() {
      
    }
  }
  
  def spec = new TestLocationSpec
  class TestLocationSpec extends Spec with FixtureServices {
    val suiteTypeName = "org.scalatest.events.LocationMethodSuiteProp$TestLocationSpec"
    val expectedStartingList = List(TestStartingPair("test succeed", "test$u0020succeed"), 
                                TestStartingPair("test pending", "test$u0020pending"), 
                                TestStartingPair("test cancel", "test$u0020cancel"))
    val expectedResultList = List(TestResultPair(classOf[TestSucceeded], "test$u0020succeed"), 
                              TestResultPair(classOf[TestPending], "test$u0020pending"),
                              TestResultPair(classOf[TestCanceled], "test$u0020cancel"),
                              TestResultPair(classOf[TestIgnored], "test$u0020ignore"))
    
    def `test succeed` {
      
    }
    def `test pending` {
      pending
    }
    def `test cancel` {
      cancel
    }
    @Ignore
    def `test ignore` {
      
    }
  }
  
  def junit3Suite = new TestLocationMethodJUnit3Suite
  
  def junitSuite = new TestLocationMethodJUnitSuite
  
  def testngSuite = new TestLocationMethodTestNGSuite
}