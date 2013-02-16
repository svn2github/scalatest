package org.scalatest

import org.scalatest._
import matchers.MatchResult

trait AMatcher[T] extends Function1[T, MatchResult] { 
  val nounName: String
  def apply(left: T): MatchResult
}

object AMatcher {
  
  def apply[T](name: String)(fun: T => Boolean) = 
    new AMatcher[T] {
      val nounName = name
      def apply(left: T): MatchResult = 
        MatchResult(
          fun(left), 
          FailureMessages("wasNotA", left, UnquotedString(nounName)), 
          FailureMessages("wasA", left, UnquotedString(nounName))
        )
    }
  
}