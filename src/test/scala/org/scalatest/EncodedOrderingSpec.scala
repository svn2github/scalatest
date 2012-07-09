package org.scalatest

import collection.immutable.TreeSet
import reflect.NameTransformer.encode

class EncodedOrderingSpec extends WordSpec {
  "EncodedOrdering" should {
    "sort unencoded strings the same as the default string ordering" in {
      val default = TreeSet("testHi", "testHo", "testPlus", "testMinus")
      val encoded = TreeSet("testHi", "testHo", "testPlus", "testMinus")(EncodedOrdering)
      assert(default.iterator.toList === encoded.iterator.toList)
    }
    "sort encoded strings in unencoded order" in {
      val set = TreeSet(encode("test: ho"), encode("test: hi"), encode("test: +"), encode("test: -"))(EncodedOrdering)
      val expected = List(encode("test: +"), encode("test: -"), encode("test: hi"), encode("test: ho"))
      assert(set.iterator.toList === expected)
    }
  }
}
