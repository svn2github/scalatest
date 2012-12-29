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
    def `for Map` {
      def `with default equality` {
        Map("I" -> 1, "II" -> 2) should === (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should !== (Map("one" -> 1, "two" -> 2))
      }
      def `with GenMap equality` {
        implicit val e = new Equality[GenMap[String,Int]] {
          def areEqual(a: GenMap[String,Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
      }
      def `with specific Map equality` {
        implicit val e = new Equality[Map[String,Int]] {
          def areEqual(a: Map[String,Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
      }
      def `with both GenMap and specific Map equality` {
        implicit val e = new Equality[GenMap[String,Int]] {
          def areEqual(a: GenMap[String,Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Map[String,Int]] { // Should pick the most specific one
          def areEqual(a: Map[String,Int], b: Any): Boolean = a != b
        }
        Map("I" -> 1, "II" -> 2) should !== (Map("I" -> 1, "II" -> 2))
        Map("I" -> 1, "II" -> 2) should === (Map("one" -> 1, "two" -> 2))
      }
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
    object `for Traversable` {
      def `with default equality` {
        Set(1, 2, 3) should === (Set(1, 2, 3))
        Set(1, 2, 3) should !== (Set(1, 2, 4))
      }
      def `with inferred GenTraversable equality` {
        // implicit val e = new Equality[GenTraversable[Int]] { ... does not and should not compile
        implicit def travEq[T <: GenTraversable[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
      def `with specific Traversable equality` {
        implicit val e = new Equality[Set[Int]] {
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
      def `with both GenTraversable and specific Traversable equality` {
        implicit val e = new Equality[GenTraversable[Int]] {
          def areEqual(a: GenTraversable[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Set[Int]] { // Should pick the most specific one
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
      def `with both inferred GenTraversable and specific Traversable equality` {
        implicit def travEq[T <: GenTraversable[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        implicit val e2 = new Equality[Set[Int]] { // Should pick the most specific one
          def areEqual(a: Set[Int], b: Any): Boolean = a != b
        }
        Set(1, 2, 3) should !== (Set(1, 2, 3))
        Set(1, 2, 3) should === (Set(1, 2, 4))
      }
    }
    def `for Java Collection` {
      pending
    }
    def `for Java Map` {
      pending
    }
    object `for Seq` {
      def `with default equality` {
        Vector(1, 2, 3) should === (Vector(1, 2, 3))
        Vector(1, 2, 3) should !== (Vector(1, 2, 4))
      }
      def `with inferred GenSeq equality` {
        // implicit val e = new Equality[GenSeq[Int]] { ... does not and should not compile
        implicit def travEq[T <: GenSeq[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
      def `with specific Seq equality` {
        implicit val e = new Equality[Vector[Int]] {
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
      def `with both GenSeq and specific Seq equality` {
        implicit val e = new Equality[GenSeq[Int]] {
          def areEqual(a: GenSeq[Int], b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Vector[Int]] { // Should pick the exact one
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
      def `with both inferred GenSeq and specific Seq equality` {
        implicit def travEq[T <: GenSeq[Int]] = new Equality[T] {
          def areEqual(a: T, b: Any): Boolean = a == b
        }
        implicit val e2 = new Equality[Vector[Int]] { // Should pick the exact one
          def areEqual(a: Vector[Int], b: Any): Boolean = a != b
        }
        Vector(1, 2, 3) should !== (Vector(1, 2, 3))
        Vector(1, 2, 3) should === (Vector(1, 2, 4))
      }
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

