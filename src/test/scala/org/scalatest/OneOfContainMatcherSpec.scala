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

class OneOfContainMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `oneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " did not contain one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(1, 3, 5, 8) should contain oneOf (7, 8, 9)
    }
    
    val matcher = new OneOfContainMatcher(List(5, 3, 8))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      List(1, 2, 3) should contain (oneOf(5, 3, 8))
      Set(1, 2, 3) should contain (oneOf(5, 3, 8))
    }
    
    def `should succeeded when right List contains at least one element in right List` {
      List(1, 2, 3) should contain oneOf (5, 3, 8)
    }
    
    def `should succeeded when right List contains more than one element in right List` {
      List(1, 2, 3) should contain oneOf (5, 3, 2)
    }
    
    def `should succeeded when right List contains all elements in left List in different order` {
      List(1, 2, 3) should contain oneOf (1, 3, 2)
    }
    
    def `should succeeded when right List contains all elements in left List in same order` {
      List(1, 2, 3) should contain oneOf (1, 2, 3)
    }
    
    def `should throw IllegalArgumentException when oneOf contains duplicate element` {
      val e = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain oneOf(6, 7, 6)
      }
      e.getMessage() should be ("oneOf must not contained duplicated value, but 6 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 6, 9)
      val left2 = Set(1, 6, 9)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (oneOf(5, 3, 8))
      }
      checkStackDepth(e3, left1, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (oneOf(5, 3, 8))
      }
      checkStackDepth(e4, left2, Array(5, 3, 8).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but does not contain any same element` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain oneOf (7, 8, 9)
      }
      checkStackDepth(e, left, Array(7, 8, 9).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List and does not contain any same element` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain oneOf (6, 7, 8, 9)
      }
      checkStackDepth(e, left, Array(6, 7, 8, 9).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List and does not contain any same element` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain oneOf (8, 5)
      }
      checkStackDepth(e, left, Array(8, 5).deep, thisLineNumber - 2)
    }
  }
  
  object `not oneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " contained one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains different elements as right List` {
      List(1, 2, 3) should not contain oneOf (7, 8, 9)
    }
    
    val matcher = new OneOfContainMatcher(List(5, 7, 9))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain oneOf (5, 7, 9)
      Set(1, 2, 8) should not contain oneOf (5, 7, 9)
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 7, 3)
      val left2 = Set(1, 7, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (5, 7, 9)
      }
      checkStackDepth(e3, left1, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (5, 7, 9)
      }
      checkStackDepth(e4, left2, Array(5, 7, 9).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain at least one same element` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain oneOf (5, 1, 7)
      }
      checkStackDepth(e, left, Array(5, 1, 7).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain more than one same element` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain oneOf (5, 1, 2)
      }
      checkStackDepth(e, left, Array(5, 1, 2).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain all same element in different order` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain oneOf (3, 2, 1)
      }
      checkStackDepth(e, left, Array(3, 2, 1).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain all same element in same order` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain oneOf (1, 2, 3)
      }
      checkStackDepth(e, left, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
  }
}
