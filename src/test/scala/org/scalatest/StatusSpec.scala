package org.scalatest

class StatusSpec extends fixture.Spec {
  
  protected type FixtureParam = { 
    def completes()
    def isCompleted: Boolean
    def succeeds(): Boolean
    def fails()
    def waitUntilCompleted()
  }
  
   override protected def withFixture(test: OneArgTest) {
     val status1 = new ScalaTestStatefulStatus
     test(status1)
     val status2 = new StatefulStatus
     test(status2)
   }
  
  object `StatefulStatus ` {
    def `should by default return false for isCompleted`(status: FixtureParam) {
      assert(!status.isCompleted)
    }
    
    def `should return true for isCompleted after completes() is called`(status: FixtureParam) {
      status.completes()
      assert(status.isCompleted)
    }
    
    def `should return true for succeeds() after completes() is called without fails()`(status: FixtureParam) {
      status.completes()
      assert(status.succeeds)
    }
    
    def `should return false for succeeds() after completes is called after fails()`(status: FixtureParam) {
      status.fails()
      status.completes()
      assert(!status.succeeds)
    }
    
    def `waitUntilCompleted should not block after completes() is called`(status: FixtureParam) {
      status.completes()
      status.waitUntilCompleted()
    }
  }
  
}