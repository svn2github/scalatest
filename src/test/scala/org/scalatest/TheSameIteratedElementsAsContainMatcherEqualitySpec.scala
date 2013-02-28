package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly

class TheSameIteratedElementsAsContainMatcherEqualitySpec extends Spec with Matchers with Explicitly with SharedHelpers {

  class TrueEquality extends Equality[Int] {
    def areEqual(left: Int, right: Any): Boolean = true
  }
  
  class FalseEquality extends Equality[Int] {
    def areEqual(left: Int, right: Any): Boolean = false
  }
  
  class MapTrueEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any): Boolean = true
  }
  
  class MapFalseEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any): Boolean = false
  }
  
  object `theSameIteratedElementsAs ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
      e.message should be (Some(leftText + " did not contain the same iterated elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
      e.message should be (Some(leftText + " contained the same iterated elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      implicit val equality = new TrueEquality
      List(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)
      Set(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)
      Array(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)
      javaList(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)
      javaSet(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)
        
      implicit val mapEquality = new MapTrueEquality
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain theSameIteratedElementsAs Map(7 -> "one", 8 -> "two", 9 -> "three")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain theSameIteratedElementsAs Map(7 -> "one", 8 -> "two", 9 -> "three")
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
      Set(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
      Array(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
      javaList(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
      javaSet(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
      
      implicit val mapEquality = new MapFalseEquality
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three"))
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new FalseEquality
      
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameIteratedElementsAs right1
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameIteratedElementsAs right2
      }
      checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val right3 = List(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain theSameIteratedElementsAs right3
      }
        checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val right4 = List(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain theSameIteratedElementsAs right4
      }
      checkShouldContainStackDepth(e4, left4, right4, thisLineNumber - 2)
        
      implicit val mapEquality = new MapFalseEquality
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain theSameIteratedElementsAs right5
      }
      checkShouldContainStackDepth(e5, left5, right5, thisLineNumber - 2)
      
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should contain theSameIteratedElementsAs right6
      }
      checkShouldContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new TrueEquality
        
      val left1 = List(1, 2, 3)
      val right1 = List(7, 8, 9)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs (right1)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val right2 = List(7, 8, 9)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs (right2)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val right3 = List(7, 8, 9)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameIteratedElementsAs (right3)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val right4 = List(7, 8, 9)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain theSameIteratedElementsAs (right4)
      }
      checkShouldNotContainStackDepth(e4, left4, right4, thisLineNumber - 2)
        
      implicit val mapEquality = new MapTrueEquality
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right5 = Map(7 -> "seven", 8 -> "eight", 9 -> "nine")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain theSameIteratedElementsAs (right5)
      }
      checkShouldNotContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right6 = Map(7 -> "seven", 8 -> "eight", 9 -> "nine")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain theSameIteratedElementsAs (right6)
      }
      checkShouldNotContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      implicit val equality = new TrueEquality
      (List(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)) (equality)
      (Set(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)) (equality)
      (Array(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)) (equality)
      (javaList(1, 2, 3) should contain theSameIteratedElementsAs List(7, 8, 9)) (equality)
       
      implicit val mapEquality = new MapTrueEquality
      (Map(1 -> "one", 2 -> "two", 3 -> "three") should contain theSameIteratedElementsAs Map(7 -> "one", 8 -> "two", 9 -> "three")) (mapEquality)
      (javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain theSameIteratedElementsAs Map(7 -> "one", 8 -> "two", 9 -> "three")) (mapEquality)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (equality)
      Set(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (equality)
      Array(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (equality)
      javaList(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (equality)
        
      implicit val mapEquality = new MapFalseEquality
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three")) (mapEquality)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain theSameIteratedElementsAs (Map(1 -> "one", 2 -> "two", 3 -> "three")) (mapEquality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new FalseEquality
        
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameIteratedElementsAs right1) (equality)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameIteratedElementsAs right2) (equality)
      }
      checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val right3 = List(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameIteratedElementsAs right3) (equality)
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val right4 = List(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain theSameIteratedElementsAs right4) (equality)
      }
      checkShouldContainStackDepth(e4, left4, right4, thisLineNumber - 2)
        
      implicit val mapEquality = new MapFalseEquality
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain theSameIteratedElementsAs right5) (mapEquality)
      }
      checkShouldContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain theSameIteratedElementsAs right6) (mapEquality)
      }
      checkShouldContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new TrueEquality
        
      val left1 = List(1, 2, 3)
      val right1 = List(7, 8, 9)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs (right1) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val right2 = List(7, 8, 9)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs (right2) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val right3 = List(7, 8, 9)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameIteratedElementsAs (right3) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val right4 = List(7, 8, 9)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain theSameIteratedElementsAs (right4) (equality)
      }
      checkShouldNotContainStackDepth(e4, left4, right4, thisLineNumber - 2)
        
      implicit val mapEquality = new MapTrueEquality
       
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right5 = Map(7 -> "seven", 8 -> "eight", 9 -> "nine")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain theSameIteratedElementsAs (right5) (mapEquality)
      }
      checkShouldNotContainStackDepth(e5, left5, right5, thisLineNumber - 2)
        
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val right6 = Map(7 -> "seven", 8 -> "eight", 9 -> "nine")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain theSameIteratedElementsAs (right6) (mapEquality)
      }
      checkShouldNotContainStackDepth(e6, left6, right6, thisLineNumber - 2)
    }
  }
  
}