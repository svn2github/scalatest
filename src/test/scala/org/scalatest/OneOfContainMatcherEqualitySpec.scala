package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import collection.GenTraversable

class OneOfContainMatcherEqualitySpec extends Spec with Matchers with Explicitly with SharedHelpers  {

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
  
  object `oneOf ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OneOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OneOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      implicit val equality = new TrueEquality
      List(1, 2, 3) should contain oneOf (2, 6, 8)
      Set(1, 2, 3) should contain oneOf (2, 6, 8)
      Array(1, 2, 3) should contain oneOf (2, 6, 8)
      javaList(1, 2, 3) should contain oneOf (2, 6, 8)
      javaSet(1, 2, 3) should contain oneOf (2, 6, 8)
        
      implicit val mapEquality = new MapTrueEquality
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain oneOf (1, 6, 8)
      Set(1, 2, 3) should not contain oneOf (1, 6, 8)
      Array(1, 2, 3) should not contain oneOf (1, 6, 8)
      javaList(1, 2, 3) should not contain oneOf (1, 6, 8)
      javaSet(1, 2, 3) should not contain oneOf (1, 6, 8)
      
      implicit val mapEquality = new MapFalseEquality
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new FalseEquality
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (1, 6, 8)
      }
      checkShouldContainStackDepth(e1, left1, Array(1, 6, 8).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (1, 6, 8)
      }
      checkShouldContainStackDepth(e2, left2, Array(1, 6, 8).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (1, 6, 8)
      }
        checkShouldContainStackDepth(e3, left3, Array(1, 6, 8).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (1, 6, 8)
      }
      checkShouldContainStackDepth(e4, left4, Array(1, 6, 8).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapFalseEquality
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (1 -> "one", 6 -> "six", 8 -> "eight")
      }
      checkShouldContainStackDepth(e5, left5, Array(1 -> "one", 6 -> "six", 8 -> "eight").deep, thisLineNumber - 2)
      
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should contain oneOf (1 -> "one", 6 -> "six", 8 -> "eight")
      }
      checkShouldContainStackDepth(e6, left6, Array(1 -> "one", 6 -> "six", 8 -> "eight").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new TrueEquality
        
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (7, 8, 9)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (7, 8, 9)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (7, 8, 9)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (7, 8, 9)
      }
      checkShouldNotContainStackDepth(e4, left4, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapTrueEquality
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkShouldNotContainStackDepth(e5, left5, Array(7 -> "seven", 8 -> "eight", 9 -> "nine").deep, thisLineNumber - 2)
        
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkShouldNotContainStackDepth(e6, left6, Array(7 -> "seven", 8 -> "eight", 9 -> "nine").deep, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      implicit val equality = new TrueEquality
      (List(1, 2, 3) should contain oneOf (7, 8, 9)) (equality)
      (Set(1, 2, 3) should contain oneOf (7, 8, 9)) (equality)
      (Array(1, 2, 3) should contain oneOf (7, 8, 9)) (equality)
      (javaList(1, 2, 3) should contain oneOf (7, 8, 9)) (equality)
       
      implicit val mapEquality = new MapTrueEquality
      (Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")) (mapEquality)
      (javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")) (mapEquality)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain oneOf (1, 2, 3) (equality)
      Set(1, 2, 3) should not contain oneOf (1, 2, 3) (equality)
      Array(1, 2, 3) should not contain oneOf (1, 2, 3) (equality)
      javaList(1, 2, 3) should not contain oneOf (1, 2, 3) (equality)
        
      implicit val mapEquality = new MapFalseEquality
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (1 -> "one", 2 -> "two", 3 -> "three") (mapEquality)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (1 -> "one", 2 -> "two", 3 -> "three") (mapEquality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new FalseEquality
        
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain oneOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain oneOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain oneOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain oneOf (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e4, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapFalseEquality
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")) (mapEquality)
      }
      checkShouldContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
        
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")) (mapEquality)
      }
      checkShouldContainStackDepth(e6, left6, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new TrueEquality
        
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (7, 8, 9) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (7, 8, 9) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (7, 8, 9) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (7, 8, 9) (equality)
      }
      checkShouldNotContainStackDepth(e4, left4, Array(7, 8, 9).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapTrueEquality
       
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine") (mapEquality)
      }
      checkShouldNotContainStackDepth(e5, left5, Array(7 -> "seven", 8 -> "eight", 9 -> "nine").deep, thisLineNumber - 2)
        
      val left6 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine") (mapEquality)
      }
      checkShouldNotContainStackDepth(e6, left6, Array(7 -> "seven", 8 -> "eight", 9 -> "nine").deep, thisLineNumber - 2)
    }
  }
  
}