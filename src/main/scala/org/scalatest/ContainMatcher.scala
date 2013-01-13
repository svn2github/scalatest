package org.scalatest

import collection.GenTraversable
import matchers.MatchResult

trait ContainMatcher[T] {
  
  def apply(left: GenTraversable[T]): MatchResult
  
}
