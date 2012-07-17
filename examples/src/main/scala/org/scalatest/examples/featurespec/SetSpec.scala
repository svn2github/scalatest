package org.scalatest.examples.featurespec

import org.scalatest.FeatureSpec

class SetSpec extends FeatureSpec {

  feature("A Set") {
    scenario("When is empty should have size 0") {
      assert(Set.empty.size === 0)
    }
    
    scenario("When is empty and head is invoked, should produce NoSuchElementException") {
      intercept[NoSuchElementException] {
        Set.empty.head
      }
    }
  }
}