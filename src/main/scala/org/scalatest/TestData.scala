package org.scalatest

trait TestData {
  /**
   * A <code>Map[String, Any]</code> containing objects that can be used
   * to configure the fixture and test.
   */
  def configMap: Map[String, Any] 
  /**
   * The name of this test.
   */
  def name: String
}