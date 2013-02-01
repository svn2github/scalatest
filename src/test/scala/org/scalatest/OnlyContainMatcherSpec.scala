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

import collection.GenTraversable

class OnlyContainMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `only ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      e.message should be (Some(left + " did not contain only (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains elements available in right List` {
      List(1, 2, 2, 3, 3, 3) should contain only (1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain only Map(1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    val matcher = new OnlyContainMatcher(List(1, 2, 3))
    val mapMatcherRight = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val mapMatcher = new OnlyContainMatcher(mapMatcherRight)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 2, 3, 3, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      List(1, 2, 2, 3, 3, 3) should contain (only(1, 2, 3))
      Set(1, 2, 3) should contain (only(1, 2, 3))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain (mapMatcher)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain only Map(1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    def `should succeeded when left list contains part of right list` {
      List(1, 2, 2, 3, 3, 3) should contain only (1, 2, 3, 4, 5)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain only Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
    }
    
    def `should throw IllegalArgumentException when only contains duplicate element` {
      val e = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain only (1, 2, 1)
      }
      e.getMessage() should be ("only must not contained duplicated value, but 1 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 9)
      val left2 = Set(1, 2, 9)
      val left3 = Map(1 -> "one", 2 -> "two", 9 -> "nine")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (only(1, 2, 3))
      }
      checkStackDepth(e3, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (only(1, 2, 3))
      }
      checkStackDepth(e4, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should contain (mapMatcher)
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should contain only Map(1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e6, left3, Map(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains element not in right List` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain only (1, 2)
      }
      checkStackDepth(e1, left1, Array(1, 2).deep, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain only Map(1 -> "one", 2 -> "two")
      }
      checkStackDepth(e2, left2, Map(1 -> "one", 2 -> "two"), thisLineNumber - 2)
    }
  }
  
  object `not only ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      e.message should be (Some(left + " contained only (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains element not in right List` {
      List(1, 2, 3) should not contain only (1, 2)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain only (Map(1 -> "one", 2 -> "two"))
    }
    
    val matcher = new OnlyContainMatcher(List(1, 2, 3))
    val mapMatcherRight = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val mapMatcher = new OnlyContainMatcher(mapMatcherRight)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain only (1, 2, 3)
      Set(1, 2, 8) should not contain only (1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain only (Map(1 -> "one", 2 -> "two", 3 -> "three"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 3)
      val left2 = Set(1, 2, 3)
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain only (1, 2, 3)
      }
      checkStackDepth(e3, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain only (1, 2, 3)
      }
      checkStackDepth(e4, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should not contain mapMatcher
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should not contain only (Map(1 -> "one", 2 -> "two", 3 -> "three"))
      }
      checkStackDepth(e6, left3, Map(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains only element in right List in same order` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain only (1, 2, 3)
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain only (Map(1 -> "one", 2 -> "two", 3 -> "three"))
      }
      checkStackDepth(e2, left2, Map(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains only element in right List in different order` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain only (3, 2, 1)
      }
      checkStackDepth(e1, left1, Array(3, 2, 1).deep, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain only (Map(3 -> "three", 2 -> "two", 1 -> "one"))
      }
      checkStackDepth(e2, left2, Map(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)
    }
  }
}
