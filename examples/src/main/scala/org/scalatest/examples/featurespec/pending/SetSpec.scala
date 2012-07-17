package org.scalatest.examples.featurespec.pending

import org.scalatest._

class SetSpec extends FeatureSpec {

  feature("A Set") {
    scenario("When empty should have size 0") (pending)
    
    scenario("When empty and head is invoked should produce NoSuchElementException") {
      intercept[NoSuchElementException] {
        Set.empty.head
      }
    }
  }
}