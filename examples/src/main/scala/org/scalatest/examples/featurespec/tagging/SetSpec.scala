package org.scalatest.examples.featurespec.tagging

import org.scalatest.Tag

object SlowTest extends Tag("com.mycompany.tags.SlowTest")
object DbTest extends Tag("com.mycompany.tags.DbTest")

import org.scalatest.FeatureSpec

class SetSpec extends FeatureSpec {

  feature("A Set") {
    scenario("When is empty should have size 0", SlowTest) {
      assert(Set.empty.size === 0)
    }
    
    scenario("When is empty and head is invoked, should produce NoSuchElementException", SlowTest, DbTest) {
      intercept[NoSuchElementException] {
        Set.empty.head
      }
    }
  }
}
