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

class TheSameIteratedElementsAsContainMatcherSpec extends Spec with Matchers with SharedHelpers  {

  object `theSameIteratedElementsAs ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      e.message should be (Some(left + " did not contain the same iterated elements as " + right))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(1, 2, 3) should contain theSameIteratedElementsAs List(1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain theSameIteratedElementsAs Map(1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    val matcherRight = List(1, 2, 3)
    val matcher = new TheSameIteratedElementsAsContainMatcher(matcherRight)
    val mapMatcherRight = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val mapMatcher = new TheSameIteratedElementsAsContainMatcher(mapMatcherRight)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      List(1, 2, 3) should contain (theSameIteratedElementsAs(List(1, 2, 3)))
      Set(1, 2, 3) should contain (theSameIteratedElementsAs(List(1, 2, 3)))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain (mapMatcher)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain (theSameIteratedElementsAs(Map(1 -> "one", 2 -> "two", 3 -> "three")))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains same elements in different order as right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(2, 1, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameIteratedElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(2 -> "two", 1 -> "one", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameIteratedElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 3, 2)
      val left2 = Map(1 -> "one", 2 -> "two", 8 -> "eight")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, matcherRight, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left1 should contain (theSameIteratedElementsAs(matcherRight))
      }
      checkStackDepth(e2, left1, matcherRight, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left2 should contain (mapMatcher)
      }
      checkStackDepth(e3, left2, mapMatcherRight, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (theSameIteratedElementsAs(mapMatcherRight))
      }
      checkStackDepth(e4, left2, mapMatcherRight, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 8)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameIteratedElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two", 8 -> "eight")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameIteratedElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3, 4)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameIteratedElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameIteratedElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameIteratedElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameIteratedElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
  }
  
  object `not theSameIteratedElementsAs ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      e.message should be (Some(left + " contained the same iterated elements as " + right))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains different elements as right List` {
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 8))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs (Map(1 -> "one", 2 -> "two", 8 -> "eight"))
    }
    
    def `should succeeded when left List contains less elements than right List` {
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3, 4))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"))
    }
    
    def `should succeeded when left List contains more elements than right List` {
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs (Map(1 -> "one", 2 -> "two"))
    }
    
    def `should succeeded when left List contains same elements as right List but in different order` {
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 3, 2))
    }
    
    val matcherRight = List(1, 3, 2)
    val matcher = new TheSameIteratedElementsAsContainMatcher(matcherRight)
    val mapMatcherRight = Map(1 -> "one", 3 -> "three", 2 -> "two")
    val mapMatcher = new TheSameIteratedElementsAsContainMatcher(mapMatcherRight)
    
    def `should work with ContainMatcher directly` {
      List(1, 2, 3) should not contain matcher
      List(1, 2, 3) should not contain theSameIteratedElementsAs(List(1, 3, 2))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain mapMatcher
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs(Map(1 -> "one", 3 -> "three", 2 -> "two"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain same elements in same order` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs (right1)
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs (right2)
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 3, 2)
      val left2 = Map(1 -> "one", 3 -> "three", 2 -> "two")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, matcherRight, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs(matcherRight)
      }
      checkStackDepth(e2, left1, matcherRight, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left2 should not contain mapMatcher
      }
      checkStackDepth(e3, left2, mapMatcherRight, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs(mapMatcherRight)
      }
      checkStackDepth(e4, left2, mapMatcherRight, thisLineNumber - 2)
    }
  }
  
}
