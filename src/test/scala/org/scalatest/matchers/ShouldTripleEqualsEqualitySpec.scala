/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.matchers

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import org.scalautils.Equality
import org.scalautils.TripleEquals

class ShouldTripleEqualsEqualitySpec extends Spec with NonImplicitAssertions with ShouldMatchers with TripleEquals {

  object `the === operator should use the appropriate Equality type class` {
    def `for Any` {
      () should === (())
      () should !== (7)
      implicit val e = new Equality[Unit] {
        def areEqual(a: Unit, b: Any): Boolean = a != b
      }
      () should !== (())
      () should === (7)
    }
    def `for String` {
      "hi" should === ("hi")
      "hi" should !== ("ho")
      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      "hi" should !== ("hi")
      "hi" should === ("ho")
    }
    def `for Numeric` {
      3 should === (3)
      3 should !== (4)
      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      3 should !== (3)
      3 should === (4)
    }
    // TODO: Equality[Map[String,Int]] doesn't work. Try making Equality contravariant
    // and recompile all
    def `for Map` {
      Map("I" -> 1, "II" -> 2) should === (Map("I" -> 1, "II" -> 2))
      Map("I" -> 1, "II" -> 2) should !== (Map("one" -> 1, "two" -> 2))
      implicit val e = new Equality[GenMap[String,Int]] {
        def areEqual(a: GenMap[String,Int], b: Any): Boolean = a != b
      }
      Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
      Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
    }
    def `for AnyRef` {
      case class Person(name: String)
      Person("Joe") should === (Person("Joe"))
      Person("Joe") should !== (Person("Sally"))
      implicit val e = new Equality[Person] {
        def areEqual(a: Person, b: Any): Boolean = a != b
      }
      Person("Joe") should !== (Person("Joe"))
      Person("Joe") should === (Person("Sally"))
    }
    // TODO: Equality[Set[Int]] doesn't work.
    def `for Traversable` {
      Set(1, 2, 3) should === (Set(1, 2, 3))
      Set(1, 2, 3) should !== (Set(1, 2, 4))
      implicit val e = new Equality[GenTraversable[Int]] {
        def areEqual(a: GenTraversable[Int], b: Any): Boolean = a != b
      }
      Set(1, 2, 3) should !== (Set(1, 2, 3))
      Set(1, 2, 3) should === (Set(1, 2, 4))
    }
    def `for Java Collection` {
      pending
    }
    def `for Java Map` {
      pending
    }
    // TODO: Equality[Vector[Int] doesn't work.
    def `for Seq` {
      Vector(1, 2, 3) should === (Vector(1, 2, 3))
      Vector(1, 2, 3) should !== (Vector(1, 2, 4))
      implicit val e = new Equality[GenSeq[Int]] {
        def areEqual(a: GenSeq[Int], b: Any): Boolean = a != b
      }
      Vector(1, 2, 3) should !== (Vector(1, 2, 3))
      Vector(1, 2, 3) should === (Vector(1, 2, 4))
    }
    def `for Array` {
      Array(1, 2, 3) should === (Array(1, 2, 3))
      Array(1, 2, 3) should !== (Array(1, 2, 4))
      implicit val e = new Equality[Array[Int]] {
        def areEqual(a: Array[Int], b: Any): Boolean = a.deep != b.asInstanceOf[Array[Int]].deep
      }
      Array(1, 2, 3) should !== (Array(1, 2, 3))
      Array(1, 2, 3) should === (Array(1, 2, 4))
    }
    def `for Java List` {
      pending
    }
  }
}

