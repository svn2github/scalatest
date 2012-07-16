package org.scalatest.examples.featurespec.getfixture

import org.scalatest.FeatureSpec
import collection.mutable.ListBuffer

class ExampleSpec extends FeatureSpec {

  def fixture = 
    new {
      val builder = new StringBuilder("ScalaTest is ")
      val buffer = new ListBuffer[String]
    }
  
  feature("Fixtures can be shared") {
    scenario("user learns how to share fixtures") {
      val f = fixture
      f.builder.append("easy!")
      assert(f.builder.toString === "ScalaTest is easy!")
      assert(f.buffer.isEmpty)
      f.buffer += "sweet"
    }
  
    scenario("user enjoys writing tests with shared fixtures") {
      val f = fixture
      f.builder.append("fun!")
      assert(f.builder.toString === "ScalaTest is fun!")
      assert(f.buffer.isEmpty)
    }
  }
}