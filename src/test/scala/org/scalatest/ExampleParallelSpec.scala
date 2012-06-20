package org.scalatest

@DoNotDiscover
class ExampleParallelSpec extends WordSpec with ParallelTestExecution {

  "Thing 1" should {
    "do thing 1a" in {
      Thread.sleep(1000)
    }
    "do thing 1b" in {
      info("This is info 1 in 1b")
      info("This is info 2 in 1b")
      Thread.sleep(900)
    }
    "do thing 1c" in {
      Thread.sleep(800)
    }
  }
  "Thing 2" should {
    "do thing 2a" in {
      Thread.sleep(700)
    }
    "do thing 2b" in {
      Thread.sleep(600)
    }
    "do thing 2c" in {
      Thread.sleep(500)
    }
  }
}