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

import org.scalatest.prop.TableDrivenPropertyChecks
import collection.GenTraversable
import collection.GenSeq
import collection.GenMap
import scala.annotation.tailrec

import matchers.HavePropertyMatcher
import matchers.HavePropertyMatchResult
import matchers.BePropertyMatcher
import matchers.BePropertyMatchResult

@Ignore
class InspectorShorthandsSpec extends Spec with Matchers with TableDrivenPropertyChecks with SharedHelpers {

  def examples =
    Table[Set[Int] => GenTraversable[Int]](
      ("Fun"), 
      ((set: Set[Int]) => set), 
      ((set: Set[Int]) => set.toList), 
      ((set: Set[Int]) => set.toSeq), 
      ((set: Set[Int]) => set.toArray), 
      ((set: Set[Int]) => set.toIndexedSeq), 
      ((set: Set[Int]) => Vector(set.toSeq: _*)),
      ((set: Set[Int]) => set.par), 
      ((set: Set[Int]) => set.toList.par), 
      ((set: Set[Int]) => set.toSeq.par), 
      ((set: Set[Int]) => set.toIndexedSeq.par), 
      ((set: Set[Int]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[Int]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[Int]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[Int]) => collection.mutable.IndexedSeq(set.toSeq: _*)), 
      ((set: Set[Int]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[Int]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[Int]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[Int]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def nullableExamples = 
    Table[Set[String] => GenTraversable[String]](
      ("Fun"), 
      ((set: Set[String]) => set), 
      ((set: Set[String]) => set.toList), 
      ((set: Set[String]) => set.toSeq), 
      ((set: Set[String]) => set.toArray[String]), 
      ((set: Set[String]) => set.toIndexedSeq), 
      ((set: Set[String]) => Vector(set.toSeq: _*)),
      ((set: Set[String]) => set.par), 
      ((set: Set[String]) => set.toList.par), 
      ((set: Set[String]) => set.toSeq.par), 
      ((set: Set[String]) => set.toIndexedSeq.par), 
      ((set: Set[String]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[String]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[String]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[String]) => collection.mutable.IndexedSeq(set.toSeq: _*)), 
      ((set: Set[String]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[String]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[String]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[String]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def traversableExamples = 
    Table[Set[Set[String]] => GenTraversable[GenTraversable[String]]](
      ("Fun"), 
      ((set: Set[Set[String]]) => set), 
      ((set: Set[Set[String]]) => set.toList), 
      ((set: Set[Set[String]]) => set.toSeq), 
      ((set: Set[Set[String]]) => set.toArray[GenTraversable[String]]), 
      ((set: Set[Set[String]]) => set.toIndexedSeq), 
      ((set: Set[Set[String]]) => Vector(set.toSeq: _*)),
      ((set: Set[Set[String]]) => set.par), 
      ((set: Set[Set[String]]) => set.toList.par), 
      ((set: Set[Set[String]]) => set.toSeq.par), 
      ((set: Set[Set[String]]) => set.toIndexedSeq.par), 
      ((set: Set[Set[String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[Set[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[Set[String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[Set[String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[Set[String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[Set[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[Set[String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[Set[String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def seqExamples = 
    Table[Set[GenSeq[String]] => GenTraversable[GenSeq[String]]](
      ("Fun"), 
      ((set: Set[GenSeq[String]]) => set), 
      ((set: Set[GenSeq[String]]) => set.toList), 
      ((set: Set[GenSeq[String]]) => set.toSeq), 
      ((set: Set[GenSeq[String]]) => set.toArray[GenSeq[String]]), 
      ((set: Set[GenSeq[String]]) => set.toIndexedSeq), 
      ((set: Set[GenSeq[String]]) => Vector(set.toSeq: _*)),
      ((set: Set[GenSeq[String]]) => set.par), 
      ((set: Set[GenSeq[String]]) => set.toList.par), 
      ((set: Set[GenSeq[String]]) => set.toSeq.par), 
      ((set: Set[GenSeq[String]]) => set.toIndexedSeq.par), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[GenSeq[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[GenSeq[String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[GenSeq[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[GenSeq[String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def mapExamples = 
    Table[Set[GenMap[String, String]] => GenTraversable[GenMap[String, String]]](
      ("Fun"), 
      ((set: Set[GenMap[String, String]]) => set), 
      ((set: Set[GenMap[String, String]]) => set.toList), 
      ((set: Set[GenMap[String, String]]) => set.toSeq), 
      ((set: Set[GenMap[String, String]]) => set.toArray[GenMap[String, String]]), 
      ((set: Set[GenMap[String, String]]) => set.toIndexedSeq), 
      ((set: Set[GenMap[String, String]]) => Vector(set.toSeq: _*)),
      ((set: Set[GenMap[String, String]]) => set.par), 
      ((set: Set[GenMap[String, String]]) => set.toList.par), 
      ((set: Set[GenMap[String, String]]) => set.toSeq.par), 
      ((set: Set[GenMap[String, String]]) => set.toIndexedSeq.par), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[GenMap[String, String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[GenMap[String, String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def javaColExamples = 
    Table[Set[java.util.Collection[String]] => GenTraversable[java.util.Collection[String]]](
      ("Fun"), 
      ((set: Set[java.util.Collection[String]]) => set), 
      ((set: Set[java.util.Collection[String]]) => set.toList), 
      ((set: Set[java.util.Collection[String]]) => set.toSeq), 
      ((set: Set[java.util.Collection[String]]) => set.toArray[java.util.Collection[String]]), 
      ((set: Set[java.util.Collection[String]]) => set.toIndexedSeq), 
      ((set: Set[java.util.Collection[String]]) => Vector(set.toSeq: _*)),
      ((set: Set[java.util.Collection[String]]) => set.par), 
      ((set: Set[java.util.Collection[String]]) => set.toList.par), 
      ((set: Set[java.util.Collection[String]]) => set.toSeq.par), 
      ((set: Set[java.util.Collection[String]]) => set.toIndexedSeq.par), 
      ((set: Set[java.util.Collection[String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[java.util.Collection[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[java.util.Collection[String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[java.util.Collection[String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[java.util.Collection[String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[java.util.Collection[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[java.util.Collection[String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[java.util.Collection[String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
  
  def javaMapExamples = 
    Table[Set[java.util.Map[String, String]] => GenTraversable[java.util.Map[String, String]]](
      ("Fun"), 
      ((set: Set[java.util.Map[String, String]]) => set), 
      ((set: Set[java.util.Map[String, String]]) => set.toList), 
      ((set: Set[java.util.Map[String, String]]) => set.toSeq), 
      ((set: Set[java.util.Map[String, String]]) => set.toArray[java.util.Map[String, String]]), 
      ((set: Set[java.util.Map[String, String]]) => set.toIndexedSeq), 
      ((set: Set[java.util.Map[String, String]]) => Vector(set.toSeq: _*)),
      ((set: Set[java.util.Map[String, String]]) => set.par), 
      ((set: Set[java.util.Map[String, String]]) => set.toList.par), 
      ((set: Set[java.util.Map[String, String]]) => set.toSeq.par), 
      ((set: Set[java.util.Map[String, String]]) => set.toIndexedSeq.par), 
      ((set: Set[java.util.Map[String, String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[java.util.Map[String, String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[java.util.Map[String, String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[java.util.Map[String, String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[java.util.Map[String, String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[java.util.Map[String, String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[java.util.Map[String, String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[java.util.Map[String, String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
  
  object `all ` {
    
    def `should pass when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        all(col) should be < 4 
      }
    }
  
    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not equal 2 
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("forAll failed, because: \n" +
                                   "  at index " + getIndex(col, 2) + ", 2 equaled 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + col))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 10))
            tfe.message should be (Some("2 equaled 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    
    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be < 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ >= 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not less than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not less than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should equal (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ == 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not equal 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not equal 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not equal (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ != 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " equaled 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " equaled 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ == 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ == 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be less than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be < 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ < 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not less than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not less than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be less than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be < (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ < 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was less than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was less than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be less than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be <= 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ <= 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not less than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not less than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be less than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be <= (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ <= 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was less than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was less than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be greater than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be > 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ > 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not greater than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not greater than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be greater than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be > (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ > 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was greater than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was greater than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be greater than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be >= 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ >= 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not greater than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not greater than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be greater than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be >= (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ >= 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was greater than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was greater than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    def `should throw TestFailedException with correct stack depth and message when 'be triple equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be === 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ == 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    def `should throw TestFailedException with correct stack depth and message when 'be not triple equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be === (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ == 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be null' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1", "2", "3"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be (null)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = col.head
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not null (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not null"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be null' failed` {
      forAll(nullableExamples) { colFun => 
        val col = 
          try {
            Some(colFun(Set("1", null, "3")))
          }
          catch {
            case iae: IllegalArgumentException =>
              // Some collection cannot contains null value, e.g. mutable.Set
              None
          }
        
        col match {
          case Some(col) => 
            val e2 = intercept[exceptions.TestFailedException] {
              all(col) should not be (null)
            }
            e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
            val firstViolation = getFirst[String](col, _ == null)
            e2.message should be (Some("forAll failed, because: \n" +
                                       "  at index " + getIndex(col, firstViolation) + ", The reference was null (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                       "in " + col))
            e2.getCause match {
              case tfe: exceptions.TestFailedException =>
                tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
                tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
                tfe.message should be (Some("The reference was null"))
                tfe.getCause should be (null)
              case other => fail("Expected cause to be TestFailedException, but got: " + other)
            }
            
          case None => // Do nothing when the collection cannot contains null value.
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("peace 1", "", "peace 2"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    class EmptyBePropertyMatcher extends BePropertyMatcher[String] {
      def apply(left: String) = BePropertyMatchResult(left.isEmpty, "empty")
    }
    val empty = new EmptyBePropertyMatcher()
    
    def `should throw TestFailedException with correct stack depth and message when 'be property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be a symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be a 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be a symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be a ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be a property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be a empty
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be a property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be a (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be an symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be an 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be an symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be an ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be an property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be an empty
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be an property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be an (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be theSameInstanceAs' failed` {
      val theInstance = "2"
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1", "2", "3"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be theSameInstanceAs theInstance
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _ eq theInstance)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not the same instance as \"" + theInstance + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not the same instance as \"" + theInstance + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be theSameInstanceAs' failed` {
      val theInstance = "2"
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1", theInstance, "3"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be theSameInstanceAs (theInstance)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _ eq theInstance)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was the same instance as \"" + theInstance + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was the same instance as \"" + theInstance + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    class StringLengthMatcher(expectedValue: Int) extends HavePropertyMatcher[String, Int] {
      def apply(value: String) = {
        new HavePropertyMatchResult(value.length == expectedValue, "length", expectedValue, value.length)
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'have property' failed` {
      def length(expectedValue: Int) = new StringLengthMatcher(expectedValue)
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have (length(0))
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", The length property had value " + firstViolation.length + ", instead of its expected value 0, on object \"" + firstViolation + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("The length property had value " + firstViolation.length + ", instead of its expected value 0, on object \"" + firstViolation + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not have property' failed` {
      def length(expectedValue: Int) = new StringLengthMatcher(expectedValue)
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have length(5)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", The length property had its expected value 5, on object \"" + firstViolation + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("The length property had its expected value 5, on object \"" + firstViolation + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'have length' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have length (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not have length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not have length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not have length' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have length (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" had length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" had length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'have size' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not have size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not have size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not have size' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" had size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" had size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string startWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should startWith ("hello")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.startsWith("hello"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not start with substring \"hello\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not start with substring \"hello\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not startWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not startWith ("hello")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.startsWith("hello"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" started with substring \"hello\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" started with substring \"hello\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string endWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks", "hi folks", "hai girls"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should endWith ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.endsWith("folks"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not end with substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not end with substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not endWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks", "hi folks", "hai girls"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not endWith ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.endsWith("folks"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" ended with substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" ended with substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string include' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should include ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not include substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not include substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not include' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not include ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" included substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" included substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string startWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should startWith regex "hel*o"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.startsWith("hello"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not start with a substring that matched the regular expression hel*o (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not start with a substring that matched the regular expression hel*o"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not startWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not startWith regex ("hel*o")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.startsWith("hello"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" started with a substring that matched the regular expression hel*o (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" started with a substring that matched the regular expression hel*o"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string endWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should endWith regex "folks!"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.endsWith("folks!"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not end with a substring that matched the regular expression folks! (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not end with a substring that matched the regular expression folks!"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not endWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not endWith regex ("folks!")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.endsWith("folks!"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" ended with a substring that matched the regular expression folks! (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" ended with a substring that matched the regular expression folks!"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string include regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should include regex "folks"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not include substring that matched regex folks (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not include substring that matched regex folks"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not include regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not include regex ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" included substring that matched regex folks (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" included substring that matched regex folks"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string fullyMatch regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1.23", "-5", "8a"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should fullyMatch regex """(-)?(\d+)(\.\d*)?"""
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.matches("""(-)?(\d+)(\.\d*)?"""))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + """" did not fully match the regular expression (-)?(\d+)(\.\d*)? (InspectorShorthandsSpec.scala:""" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + """" did not fully match the regular expression (-)?(\d+)(\.\d*)?"""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not fullyMatch regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1.23", "-5", "8a"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.matches("""(-)?(\d+)(\.\d*)?"""))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + """" fully matched the regular expression (-)?(\d+)(\.\d*)? (InspectorShorthandsSpec.scala:""" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + """" fully matched the regular expression (-)?(\d+)(\.\d*)?"""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable be symbol' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenTraversable[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable not be symbol' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenTraversable[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'seq have length' failed` {
      forAll(seqExamples) { colFun => 
        val col = colFun(Set(Seq(), Seq("boom!"), Seq()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have length 0
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenSeq[String]](col, _.length == 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not have length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not have length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'seq not have length' failed` {
      forAll(seqExamples) { colFun => 
        val col = colFun(Set(Seq(), Seq("boom!"), Seq()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have length (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenSeq[String]](col, _.length == 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " had length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " had length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable have size' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have size 0
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenTraversable[String]](col, _.size == 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not have size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not have size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable not have size' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenTraversable[String]](col, _.size == 0)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " had size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " had size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable contain' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set("1", "2", "3"), Set("4", "5", "6"), Set("2", "6", "8")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain ("2")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenTraversable[String]](col, _.exists(_ == "2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not contain element \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not contain element \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable not contain' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set("1", "2", "3"), Set("4", "5", "6"), Set("2", "6", "8")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain "2"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenTraversable[String]](col, _.exists(_ == "2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " contained element \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " contained element \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map contain key' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain key "2"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenMap[String, String]](col, _.exists(_._1 == "2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not contain key \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " did not contain key \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map not contain key' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain key ("2")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenMap[String, String]](col, _.exists(_._1 == "2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " contained key \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " contained key \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map contain value' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain value "two"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenMap[String, String]](col, _.exists(_._2 == "two"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not contain value \"two\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " did not contain value \"two\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map not contain value' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain value ("two")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenMap[String, String]](col, _.exists(_._2 == "two"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " contained value \"two\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " contained value \"two\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    private def javaCol(valueSet: GenTraversable[String]): java.util.Collection[String] = {
      val javaCol = new java.util.ArrayList[String]()
      for (value <- valueSet)
        javaCol.add(value)
      javaCol
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection be symbol' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Set()), javaCol(Set("boom!")), javaCol(Set())))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Collection[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection not be symbol' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Set()), javaCol(Set("boom!")), javaCol(Set())))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Collection[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection have size' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Seq()), javaCol(Seq("boom!")), javaCol(Seq())))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have size 0
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Collection[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not have size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not have size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection not have size' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Seq()), javaCol(Seq("boom!")), javaCol(Seq())))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Collection[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " had size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " had size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection have length' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Seq()), javaCol(Seq("boom!")), javaCol(Seq())))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have length 0
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Collection[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not have length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not have length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection not have length' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Seq()), javaCol(Seq("boom!")), javaCol(Seq())))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have length (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Collection[String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " had length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " had length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection contain' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Set("1", "2", "3")), javaCol(Set("4", "5", "6")), javaCol(Set("2", "6", "8"))))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain ("2")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Collection[String]](col, _.contains("2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not contain element \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not contain element \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java collection not contain' failed` {
      forAll(javaColExamples) { colFun => 
        val col = colFun(Set(javaCol(Set("1", "2", "3")), javaCol(Set("4", "5", "6")), javaCol(Set("2", "6", "8"))))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain "2"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Collection[String]](col, _.contains("2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " contained element \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " contained element \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    private def javaMap(valueMap: Map[String, String]): java.util.Map[String, String] = {
      val javaMap = new java.util.HashMap[String, String]()
      for ((key, value) <- valueMap)
        javaMap.put(key, value)
      javaMap
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map be symbol' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map.empty), javaMap(Map("b" -> "boom!")), javaMap(Map.empty)))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Map[String, String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map not be symbol' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map.empty), javaMap(Map("b" -> "boom!")), javaMap(Map.empty)))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Map[String, String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map have size' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map.empty), javaMap(Map("b" -> "boom!")), javaMap(Map.empty)))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have size 0
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Map[String, String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not have size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not have size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map not have size' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map.empty), javaMap(Map("b" -> "boom!")), javaMap(Map.empty)))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Map[String, String]](col, _.isEmpty)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " had size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " had size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map contain key' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map("1" -> "one", "2" -> "two", "3" -> "three")), javaMap(Map("4" -> "four", "5" -> "five", "6" -> "six")), javaMap(Map("2" -> "two", "6" -> "six", "8" -> "eight"))))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain key "2"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Map[String, String]](col, _.containsKey("2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not contain key \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " did not contain key \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map not contain key' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map("1" -> "one", "2" -> "two", "3" -> "three")), javaMap(Map("4" -> "four", "5" -> "five", "6" -> "six")), javaMap(Map("2" -> "two", "6" -> "six", "8" -> "eight"))))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain key ("2")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Map[String, String]](col, _.containsKey("2"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " contained key \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " contained key \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map contain value' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map("1" -> "one", "2" -> "two", "3" -> "three")), javaMap(Map("4" -> "four", "5" -> "five", "6" -> "six")), javaMap(Map("2" -> "two", "6" -> "six", "8" -> "eight"))))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain value "two"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[java.util.Map[String, String]](col, _.containsValue("two"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not contain value \"two\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " did not contain value \"two\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'java map not contain value' failed` {
      forAll(javaMapExamples) { colFun => 
        val col = colFun(Set(javaMap(Map("1" -> "one", "2" -> "two", "3" -> "three")), javaMap(Map("4" -> "four", "5" -> "five", "6" -> "six")), javaMap(Map("2" -> "two", "6" -> "six", "8" -> "eight"))))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain value ("two")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[java.util.Map[String, String]](col, _.containsValue("two"))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " contained value \"two\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation.toString + " contained value \"two\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
  }
}
