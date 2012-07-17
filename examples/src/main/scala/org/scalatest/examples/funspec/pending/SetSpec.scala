package org.scalatest.examples.funspec.pending

import org.scalatest._

class SetSpec extends FunSpec {

  describe("An empty Set") {
    it("should have size 0") (pending)
    
    it("should produce NoSuchElementException when head is invoked") {
      intercept[NoSuchElementException] {
        Set.empty.head
      }
    }
  }
}