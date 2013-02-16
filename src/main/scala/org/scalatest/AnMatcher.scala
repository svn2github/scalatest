package org.scalatest

import org.scalatest._
import matchers.MatchResult

trait AnMatcher[T] extends Function1[T, MatchResult] { 
  val nounName: String
  def apply(left: T): MatchResult
}

object AnMatcher {
  
  def apply[T](name: String)(fun: T => Boolean) = 
    new AnMatcher[T] {
      val nounName = name
      def apply(left: T): MatchResult = 
        MatchResult(
          fun(left), 
          FailureMessages("wasNotAn", left, UnquotedString(nounName)), 
          FailureMessages("wasAn", left, UnquotedString(nounName))
        )
    }
  
}