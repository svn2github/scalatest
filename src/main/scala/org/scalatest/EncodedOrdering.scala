package org.scalatest

import scala.reflect.NameTransformer.decode

private[scalatest] object EncodedOrdering extends Ordering[String] {
  def compare(x: String, y: String): Int = {
    decode(x) compareTo decode(y)
  }
}
