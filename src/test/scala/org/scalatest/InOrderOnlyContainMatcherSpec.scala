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

class InOrderOnlyContainMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `inOrderOnly ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " did not contain only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains elements available in right List` {
      List(4, 4, 4, 5, 5, 6, 6) should contain inOrderOnly (4, 5, 6)
    }
    
    val matcher = new InOrderOnlyContainMatcher(List(1, 2, 3))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 2, 3, 3, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      List(1, 2, 2, 3, 3, 3) should contain (inOrderOnly(1, 2, 3))
      Set(1, 2, 3) should contain (inOrderOnly(1, 2, 3))
    }
    
    def `should succeed when left list contains part of right list` {
      List(1, 2, 2, 3, 3, 3) should contain inOrderOnly (1, 2, 3, 4, 5)
    }
    
    def `should throw IllegalArgumentException when inOrderOnly contains duplicate element` {
      val e = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain inOrderOnly (1, 2, 1)
      }
      e.getMessage() should be ("inOrderOnly must not contained duplicated value, but 1 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 9)
      val left2 = Set(1, 2, 9)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (inOrderOnly(1, 2, 3))
      }
      checkStackDepth(e3, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (inOrderOnly(1, 2, 3))
      }
      checkStackDepth(e4, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains element not in right List` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain inOrderOnly (1, 2)
      }
      checkStackDepth(e, left, Array(1, 2).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains only element in right List, but in different order` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain inOrderOnly (3, 2, 1)
      }
      checkStackDepth(e, left, Array(3, 2, 1).deep, thisLineNumber - 2)
    }
  }
  
  object `not inOrderOnly ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " contained only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains element not in right List` {
      List(1, 2, 3) should not contain inOrderOnly (1, 2)
    }
    
    def `should succeed when left List contains element in right List but in different order` {
      List(1, 2, 3) should not contain inOrderOnly (3, 2, 1)
    }
    
    val matcher = new InOrderOnlyContainMatcher(List(1, 2, 3))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain inOrderOnly (3, 2, 1)
      Set(1, 2, 8) should not contain inOrderOnly (3, 2, 1)
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 3)
      val left2 = Set(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrderOnly (1, 2, 3)
      }
      checkStackDepth(e3, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrderOnly (1, 2, 3)
      }
      checkStackDepth(e4, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains only element in right List in same order` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain inOrderOnly (1, 2, 3)
      }
      checkStackDepth(e, left, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
  }
}
