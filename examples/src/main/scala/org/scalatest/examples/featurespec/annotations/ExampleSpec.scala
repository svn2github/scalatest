package org.scalatest.examples.featurespec.annotations

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FeatureSpec

class ExampleSpec extends FeatureSpec {

  feature("A calculator can do addition and subtraction") {
    
    scenario("When 1 + 1 is entered, the result should be 2", SlowTest) {
      val sum = 1 + 1
      assert(sum === 2)
    }
    
    scenario("When 4 - 1 is entered, the result should be 3", SlowTest, DbTest) {
      val diff = 4 - 1
      assert(diff === 3)
    }
  }
}