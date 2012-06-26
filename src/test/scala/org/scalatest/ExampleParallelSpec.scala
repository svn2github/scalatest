package org.scalatest

@DoNotDiscover
class ExampleParallelSpec extends WordSpec with ParallelTestExecution {

  "Subject 1" should {
    "have behavior 1a" in {
      Thread.sleep(1000)
    }
    "have behavior 1b" in {
      info("This is info 1 in 1b")
      info("This is info 2 in 1b")
      Thread.sleep(900)
    }
    "have behavior 1c" in {
      Thread.sleep(800)
    }
  }
  "Subject 2" should {
    "have behavior 2a" in {
      Thread.sleep(700)
    }
    "have behavior 2b" in {
      Thread.sleep(600)
    }
    "have behavior 2c" in {
      Thread.sleep(500)
    }
  }
}