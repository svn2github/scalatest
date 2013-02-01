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

class TheSameElementsAsContainMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `theSameElementsAs ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("TheSameElementsAsContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(1, 2, 3) should contain theSameElementsAs List(1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain theSameElementsAs Map(1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    val matcherRight = List(1, 2, 3)
    val matcher = new TheSameElementsAsContainMatcher(matcherRight)
    val mapMatcherRight = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val mapMatcher = new TheSameElementsAsContainMatcher(mapMatcherRight)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      List(1, 2, 3) should contain (theSameElementsAs(List(1, 2, 3)))
      Set(1, 2, 3) should contain (theSameElementsAs(List(1, 2, 3)))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain (mapMatcher)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain (theSameElementsAs(Map(1 -> "one", 2 -> "two", 3 -> "three")))
    }
    
    def `should succeeded when left List contains same elements in different order as right List` {
      List(1, 2, 3) should contain theSameElementsAs List(2, 1, 3)
    }
    
    def `should succeeded when left List contains same elements in different order as right Set` {
      List(1, 2, 3) should contain theSameElementsAs Set(2, 1, 3)
    }
    
    def `should succeeded when left List contains same elements in same order as right Set` {
      List(1, 2, 3) should contain theSameElementsAs Set(1, 2, 3)
    }
    
    def `should succeeded when left Map contains same elements as right Map` {
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain theSameElementsAs Map(1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 8)
      val left2 = Set(1, 2, 8)
      val left3 = Map(1 -> "one", 2 -> "two", 8 -> "eight")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, matcherRight, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, matcherRight, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (theSameElementsAs(matcherRight))
      }
      checkStackDepth(e3, left1, matcherRight, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (theSameElementsAs(matcherRight))
      }
      checkStackDepth(e4, left2, matcherRight, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should contain (mapMatcher)
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should contain (theSameElementsAs(mapMatcherRight))
      }
      checkStackDepth(e6, left3, mapMatcherRight, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements` {
      val left1 = List(1, 2, 3)
      val right1 = List(2, 5, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(2 -> "two", 5 -> "five", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3, 4)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List and right Set are same size but contain different elements` {
      val left1 = List(1, 2, 3)
      val right1 = Set(2, 5, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(2 -> "two", 5 -> "five", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right Set` {
      val left1 = List(1, 2, 3)
      val right1 = Set(1, 2, 3, 4)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right Set` {
      val left1 = List(1, 2, 3)
      val right1 = Set(1, 2)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(1 -> "one", 2 -> "two")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List does not contain all repeated elements in right List` {
      val left1 = List(1, 1, 2)
      val right1 = List(1, 2, 2)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
    }
  }
  
  object `not theSameElementsAs ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      e.message should be (Some(left + " contained the same elements as " + right))
      e.failedCodeFileName should be (Some("TheSameElementsAsContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains different elements as right List` {
      List(1, 2, 3) should not contain theSameElementsAs (List(1, 2, 8))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameElementsAs (Map(1 -> "one", 2 -> "two", 8 -> "eight"))
    }
    
    val matcherRight = List(1, 2, 3)
    val matcher = new TheSameElementsAsContainMatcher(matcherRight)
    val mapMatcherRight = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val mapMatcher = new TheSameElementsAsContainMatcher(mapMatcherRight)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain theSameElementsAs(List(1, 2, 3))
      Set(1, 2, 8) should not contain theSameElementsAs(List(1, 2, 3))
      
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain theSameElementsAs(Map(1 -> "one", 2 -> "two", 3 -> "three"))
    }
    
    def `should succeeded when left List contains different elements in different order as right List` {
      List(1, 2, 3) should not contain theSameElementsAs (List(2, 1, 8))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameElementsAs (Map(2 -> "two", 1 -> "one", 8 -> "eight"))
    }
    
    def `should succeeded when left List contains different elements in different order as right Set` {
      List(1, 2, 3) should not contain theSameElementsAs (Set(2, 1, 8))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameElementsAs (Map(2 -> "two", 1 -> "one", 8 -> "eight"))
    }
    
    def `should succeeded when left List contains different elements in same order as right Set` {
      List(1, 2, 3) should not contain theSameElementsAs (Set(1, 2, 8))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameElementsAs (Map(1 -> "one", 2 -> "two", 8 -> "eight"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 3)
      val left2 = Set(1, 2, 3)
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, matcherRight, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, matcherRight, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameElementsAs(matcherRight)
      }
      checkStackDepth(e3, left1, matcherRight, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameElementsAs(matcherRight)
      }
      checkStackDepth(e4, left2, matcherRight, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should not contain mapMatcher
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameElementsAs(mapMatcherRight)
      }
      checkStackDepth(e6, left3, mapMatcherRight, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain same elements in different order` {
      val left1 = List(1, 2, 3)
      val right1 = List(2, 1, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameElementsAs (right1)
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right2 = Map(2 -> "two", 1 -> "one", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameElementsAs (right2)
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List and right Set are same size but contain same elements in different order` {
      val left1 = List(2, 3, 5)
      val right1 = Set(2, 5, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameElementsAs (right1)
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = Map(2 -> "two", 3 -> "three", 5 -> "five")
      val right2 = Map(2 -> "two", 5 -> "five", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameElementsAs (right2)
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
    }
    
  }
  
}
