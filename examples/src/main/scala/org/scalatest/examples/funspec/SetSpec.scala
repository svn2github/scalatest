package org.scalatest.examples.funspec

import org.scalatest.FunSpec

class SetSpec extends FunSpec {

  describe("An empty Set") {
    it("should have size 0") {
      assert(Set.empty.size === 0)
    }
    
    it("should produce NoSuchElementException when head is invoked") {
      intercept[NoSuchElementException] {
        Set.empty.head
      }
    }
  }
}