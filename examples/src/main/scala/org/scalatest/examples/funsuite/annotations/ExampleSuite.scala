package org.scalatest.examples.funsuite.annotations

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

  test("addition", SlowTest) {
    val sum = 1 + 1
    assert(sum === 2)
  }

  test("subtraction", SlowTest, DbTest) {
    val diff = 4 - 1
    assert(diff === 3)
  }
}
