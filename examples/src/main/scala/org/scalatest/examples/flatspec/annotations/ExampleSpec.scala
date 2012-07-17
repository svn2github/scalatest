package org.scalatest.examples.flatspec.annotations

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FlatSpec

class ExampleSpec extends FlatSpec {

  "A calculator" should "add correctly" taggedAs(SlowTest) in {
    val sum = 1 + 1
    assert(sum === 2)
  }
    
  it should "subtract correctly" taggedAs(SlowTest, DbTest) in {
    val diff = 4 - 1
    assert(diff === 3)
  }
}