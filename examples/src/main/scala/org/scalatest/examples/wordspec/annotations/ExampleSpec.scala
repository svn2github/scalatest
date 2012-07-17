package org.scalatest.examples.wordspec.annotations

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.WordSpec

class ExampleSpec extends WordSpec {

  "A calculator" should {
    
    "add correctly" taggedAs(SlowTest) in {
      val sum = 1 + 1
      assert(sum === 2)
    }
    
    "subtract correctly" taggedAs(SlowTest, DbTest) in {
      val diff = 4 - 1
      assert(diff === 3)
    }
  }
}