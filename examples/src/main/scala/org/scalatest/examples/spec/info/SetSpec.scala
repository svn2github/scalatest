package org.scalatest.examples.spec.info

import collection.mutable
import org.scalatest._
 
class SetSpec extends Spec with GivenWhenThen {
  
  object `A mutable Set` {
    def `should allow an element to be added` {
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