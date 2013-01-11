package org.scalatest.matchers

import org.scalatest._

class TheSameElementsAsContainMatcherSpec extends Spec with ShouldMatchers with SharedHelpers {

  object `theSameElementsAs ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      e.message should be (Some(left + " did not contain the same elements as " + right))
      e.failedCodeFileName should be (Some("TheSameElementsAsContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(1, 2, 3) should contain theSameElementsAs List(1, 2, 3)
    }
    
    val matcherRight = List(1, 2, 3)
    val matcher = new TheSameElementsAsContainMatcher(matcherRight)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      List(1, 2, 3) should contain (theSameElementsAs(List(1, 2, 3)))
      Set(1, 2, 3) should contain (theSameElementsAs(List(1, 2, 3)))
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
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 8)
      val left2 = Set(1, 2, 8)
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
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements` {
      val left = List(1, 2, 3)
      val right = List(2, 5, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List` {
      val left = List(1, 2, 3)
      val right = List(1, 2, 3, 4)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List` {
      val left = List(1, 2, 3)
      val right = List(1, 2)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List and right Set are same size but contain different elements` {
      val left = List(1, 2, 3)
      val right = Set(2, 5, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right Set` {
      val left = List(1, 2, 3)
      val right = Set(1, 2, 3, 4)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right Set` {
      val left = List(1, 2, 3)
      val right = Set(1, 2)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List does not contain all repeated elements in right List` {
      val left = List(1, 1, 2)
      val right = List(1, 2, 2)
      val e = intercept[exceptions.TestFailedException] {
        left should contain theSameElementsAs right
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
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
    }
    
    val matcherRight = List(1, 2, 3)
    val matcher = new TheSameElementsAsContainMatcher(matcherRight)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain theSameElementsAs(List(1, 2, 3))
      Set(1, 2, 8) should not contain theSameElementsAs(List(1, 2, 3))
    }
    
    def `should succeeded when left List contains different elements in different order as right List` {
      List(1, 2, 3) should not contain theSameElementsAs (List(2, 1, 8))
    }
    
    def `should succeeded when left List contains different elements in different order as right Set` {
      List(1, 2, 3) should not contain theSameElementsAs (Set(2, 1, 8))
    }
    
    def `should succeeded when left List contains different elements in same order as right Set` {
      List(1, 2, 3) should not contain theSameElementsAs (Set(1, 2, 8))
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 3)
      val left2 = Set(1, 2, 3)
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
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain same elements in different order` {
      val left = List(1, 2, 3)
      val right = List(2, 1, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain theSameElementsAs (right)
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List and right Set are same size but contain same elements in different order` {
      val left = List(2, 3, 5)
      val right = Set(2, 5, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain theSameElementsAs (right)
      }
      checkStackDepth(e, left, right, thisLineNumber - 2)
    }
    
  }
  
}