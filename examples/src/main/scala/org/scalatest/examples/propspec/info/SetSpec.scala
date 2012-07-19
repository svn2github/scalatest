package org.scalatest.examples.propspec.info

import org.scalatest._
import prop._
import collection.mutable

class SetSuite extends PropSpec with TableDrivenPropertyChecks with GivenWhenThen {

  val examples =
    Table(
      "set",
      mutable.BitSet.empty,
      mutable.HashSet.empty[Int],
      mutable.LinkedHashSet.empty[Int]
    )

  property("an element can be added to an empty mutable Set") {

    forAll(examples) { set =>

      info("----------------")

      given("an empty mutable " + set.getClass.getSimpleName)
      assert(set.isEmpty)

      when("an element is added")
      set += 99

      then("the Set should have size 1")
      assert(set.size === 1)

      and("the Set should contain the added element")
      assert(set.contains(99))
    }
  }
}
