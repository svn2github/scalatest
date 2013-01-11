package org.scalatest.matchers

import org.scalatest._

class AllOfContainMatcherSpec extends Spec with ShouldMatchers with SharedHelpers {

  object `allOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " did not contain all of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("AllOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(5, 4, 3, 2, 1) should contain allOf(1, 3, 5)
    }
    
    val matcher = new AllOfContainMatcher(List(1, 2))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      List(1, 2, 3) should contain (allOf(1, 2))
      Set(1, 2, 3) should contain (allOf(1, 2))
    }
    
    def `should succeeded when left List contains same elements in different order as right List` {
      List(1, 2, 3) should contain allOf(2, 1, 3)
    }
    
    def `should succeeded when left List contains same elements in same order as right Set` {
      List(1, 2, 3) should contain allOf(1, 2, 3)
    }
    
    def `should throw IllegalArgumentException when allOf contains duplicate element` {
      val e = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain allOf(1, 2, 1)
      }
      e.getMessage() should be ("allOf must not contained duplicated value, but 1 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 3, 8)
      val left2 = Set(1, 3, 8)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(1, 2).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(1, 2).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (allOf(1, 2))
      }
      checkStackDepth(e3, left1, Array(1, 2).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (allOf(1, 2))
      }
      checkStackDepth(e4, left2, Array(1, 2).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain allOf (2, 5, 3)
      }
      checkStackDepth(e, left, Array(2, 5, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain allOf (1, 2, 3, 4)
      }
      checkStackDepth(e, left, Array(1, 2, 3, 4).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should contain allOf (1, 5)
      }
      checkStackDepth(e, left, Array(1, 5).deep, thisLineNumber - 2)
    }
  }
  
  object `not allOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: IndexedSeq[Any], lineNumber: Int) {
      e.message should be (Some(left + " contained all of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("AllOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains different elements as right List` {
      List(1, 2, 3) should not contain allOf (1, 2, 8)
    }
    
    val matcher = new AllOfContainMatcher(List(1, 2, 3))
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain allOf (1, 2, 3)
      Set(1, 2, 8) should not contain allOf (1, 2, 3)
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
        left1 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e3, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e4, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain same elements in different order` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain allOf (2, 1, 3)
      }
      checkStackDepth(e, left, Array(2, 1, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain same elements in same order` {
      val left = List(1, 2, 3)
      val e = intercept[exceptions.TestFailedException] {
        left should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e, left, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
  }
}