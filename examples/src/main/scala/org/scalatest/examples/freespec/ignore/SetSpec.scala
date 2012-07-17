package org.scalatest.examples.freespec.ignore

import org.scalatest.FreeSpec

class SetSpec extends FreeSpec {
  
  "An empty Set" - {
    "should have size 0" ignore {
      assert(Set.empty.size === 0)
    }
    
    "should produce NoSuchElementException when head is invoked" in {
      intercept[NoSuchElementException] {
        Set.empty.head
      }
    }
  }
}