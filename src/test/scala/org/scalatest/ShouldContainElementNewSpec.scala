/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest

import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import org.scalatest.exceptions.TestFailedException
import org.scalautils.Equality
import org.scalautils.Explicitly
import SharedHelpers.thisLineNumber

// Calling this ShouldContainElementNewSpec so that it is easy to 
// keep track of the new tests that we'll need to port over to
// inspector shorthands.
class ShouldContainElementNewSpec extends Spec with Matchers with Explicitly {

  // Checking for a specific size
  object `The 'contain (<value>)' syntax` {
    def `should allow subtypes of the element type to be passed in if modified by asAny` {
      Vector(1, "2") should contain ("2".asAny)
      Vector(1, "2") should contain (1.asAny)
    }
    def `should use an Equality of the element type of the left-hand "holder" on a GenTraversable` {
      
      Vector(1, 2) should contain (2)
      val e1 = intercept[TestFailedException] {
        Vector(1, 2) should not contain (2)
      }

      e1.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      
      val e2 = intercept[TestFailedException] {
        Vector(2, 2) should contain (2)
      }
      Vector(1, 1) should not contain (2)

      e2.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))

      (Vector(2, 2) should contain (2)) (traversableDecider(defaultEquality))
      val e3 = intercept[TestFailedException] {
        (Vector(1, 1) should contain (2)) (traversableDecider(defaultEquality))
      }

      e3.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      Vector(2, 2) should contain (2) (traversableDecider[Int, Vector](defaultEquality))
      val e4 = intercept[TestFailedException] {
        Vector(1, 1) should contain (2) (traversableDecider[Int, Vector](defaultEquality))
      }

      e4.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e4.failedCodeLineNumber should be (Some(thisLineNumber - 4))

/*
      (Vector(2, 2) should contain (2)) (decided by traversableDecider(defaultEquality))
      val e5 = intercept[TestFailedException] {
        (Vector(1, 1) should contain (2)) (decided by traversableDecider(defaultEquality))
      }

      e5.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e5.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      Vector(2, 2) should contain (2) (decided by traversableDecider[Int, Vector](defaultEquality))
      val e6 = intercept[TestFailedException] {
        Vector(1, 1) should contain (2) (decided by traversableDecider[Int, Vector](defaultEquality))
      }

        Vector(1, 1) should contain (2) (decidedForTraversable by defaultEquality)

      e6.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e6.failedCodeLineNumber should be (Some(thisLineNumber - 4))
*/
    }
/*
No, what should work is:

contain (7) returns a subclass of MatcherFactory1 that also has an extra apply method that takes an Equality.
This apply method returns a different MatcherFactory1 that requires not a Holder but a SpecifiedEqualityHolder
or something. And it's this:
trait SpecifiedEqualityHolder[A] {
  def containsElement(aggregation: A, element: Any, specifiedEquality: Equality[Any]): Boolean
}

Well because I don't know A yet, I'd have to say Int, and even then, this is wrong. So it would need
to be something that doesn't care about the left or right? An Equality[Any]? Or maybe it is an
existential type, but that won't work either.

Nope. So what about doing it via an implicit conversion. Wait, what about an apply method that actually
returns a Matcher[Int]?  An apply method that takes a...

What if Holder had another method on it that took a specified Equality?
Well, o

Can I say: 

decided by defaultElementEquality

And have that give me a holder?

def defaultElementEquality[T]: Holder[T] = new Holder[T] {
  def containsElement(aggregation: T, element: Any, specifiedEquality: Equality[Any]): Boolean
}

def defaultElementEquality = holder(defaultEquality) // ??
def fuzzyElementEquality = holder(fuzzyEquality) // ??

      (Vector(2, 2) should contain (2)) (decided by traversableHolder(defaultEquality))

I could say "decided using defaultEquality" and have an implicit from that to whatever T is, but again, I don't know the types.

I don't htink it will work and i don't want the implcits. 

Maybe can have a traversableDecider method that takes a specified Equality and returns something that
implements Holder, Aggregation, and Orderable. Then it would just be like this:

(Vector(2, 2) should contain (2)) (decided by traversableDecider(defaultEquality))

traversableDecider is not implicit, and neither is the Equality it takes. It is just a method. Can have
one for arrayDecider, stringDecider, javaCollectionDecider, etc.

Oh yes, and the optionDecider would only implement Holder[Option], not Aggregation[Option], etc.
*/
/*
    def `should use an Equality of the element type of the left-hand "holder" on a String` {
      
      "12" should contain ('2')
      val e1 = intercept[TestFailedException] {
        "12" should not contain ('2')
      }

      e1.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      implicit val e = new Equality[String] {
        def areEqual(a: String, b: Any): Boolean = a != b
      }
      
      val e2 = intercept[TestFailedException] {
        "12" should contain ('2')
      }
      "12" should not contain ('2')

      e2.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))
    }
*/
    def `should use an Equality of the element type of the left-hand "holder" on an Array` {
      
      Array(1, 2) should contain (2)
      val e1 = intercept[TestFailedException] {
        Array(1, 2) should not contain (2)
      }

      e1.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      
      val e2 = intercept[TestFailedException] {
        Array(2, 2) should contain (2)
      }
      Array(1, 1) should not contain (2)

      e2.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))
    }
  }
}

