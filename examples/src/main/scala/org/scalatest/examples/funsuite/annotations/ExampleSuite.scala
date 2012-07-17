package org.scalatest.examples.funsuite.annotations

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FunSuite

class ExampleSuite extends FunSuite {

  test("Addition", SlowTest) {
    val sum = 1 + 1
    assert(sum === 2)
  }

  test("Subtraction", SlowTest, DbTest) {
    val diff = 4 - 1
    assert(diff === 3)
  }
}
