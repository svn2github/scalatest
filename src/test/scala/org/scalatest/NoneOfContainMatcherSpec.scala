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

class NoneOfContainMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `noneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " contained one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains elements available in right List` {
      List(1, 2, 3, 4, 5) should contain noneOf (6, 7, 8)
    }
    
    val matcher = new NoneOfContainMatcher(List(6, 7, 8))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3, 4, 5) should contain (matcher)
      Set(1, 2, 3, 4, 5) should contain (matcher)
      
      List(1, 2, 3, 4, 5) should contain (noneOf(6, 7, 8))
      Set(1, 2, 3, 4, 5) should contain (noneOf(6, 7, 8))
    }
    
    def `should succeed when left list contains none of right list` {
      List(1, 2, 3) should contain noneOf (7, 8)
    }
    
    def `should throw IllegalArgumentException when noneOf contains duplicate element` {
      val e = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain noneOf (6, 8, 6)
      }
      e.getMessage() should be ("noneOf must not contained duplicated value, but 6 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 7)
      val left2 = Set(1, 2, 7)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (noneOf(6, 7, 8))
      }
      checkStackDepth(e3, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (noneOf(6, 7, 8))
      }
      checkStackDepth(e4, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains element in right List` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain noneOf (0, 3, 8)
      }
      checkStackDepth(e, left, Array(0, 3, 8).deep, thisLineNumber - 2)
    }
  }
  
  object `not noneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " did not contain one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains element in right List` {
      List(1, 2, 3) should not contain noneOf (0, 2, 8)
    }
    
    val matcher = new NoneOfContainMatcher(List(0, 2, 8))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain noneOf (0, 2, 8)
      Set(1, 2, 8) should not contain noneOf (0, 2, 8)
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(5, 6, 7)
      val left2 = Set(5, 6, 7)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain noneOf (0, 2, 8)
      }
      checkStackDepth(e3, left1, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain noneOf (0, 2, 8)
      }
      checkStackDepth(e4, left2, Array(0, 2, 8).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains only element in right List in same order` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain noneOf (7, 8, 9)
      }
      checkStackDepth(e, left, Array(7, 8, 9).deep, thisLineNumber - 2)
    }
  }
}
