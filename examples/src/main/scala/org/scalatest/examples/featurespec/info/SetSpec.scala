package org.scalatest.examples.featurespec.info

import collection.mutable
import org.scalatest._

class SetSpec extends FeatureSpec with GivenWhenThen {
  
  feature("An element can be added to an empty mutable Set") {
    scenario("When an element is added to an empty mutable Set") {
      given("an empty mutable Set")
      val set = mutable.Set.empty[String]

      when("an element is added")
      set += "clarity"

      then("the Set should have size 1")
      assert(set.size === 1)

      and("the Set should contain the added element")
      assert(set.contains("clarity"))

      info("That's all folks!")
    }
  }
}