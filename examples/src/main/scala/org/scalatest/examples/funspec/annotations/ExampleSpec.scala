package org.scalatest.examples.funspec.annotations

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FunSpec

class ExampleSpec extends FunSpec {

  describe("A calculator") {
    
    it("should add correctly", SlowTest) {
      val sum = 1 + 1
      assert(sum === 2)
    }
    
    it("should subtract correctly", SlowTest, DbTest) {
      val diff = 4 - 1
      assert(diff === 3)
    }
  }
}