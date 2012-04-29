package org.scalatest

import SavesConfigMapSuite.theConfigMap
import org.scalatest.exceptions._

@WrapWith(classOf[ConfigMapWrapperSuite])
class SavesConfigMapSuite(configMap: Map[String, Any]) extends FunSuite {
  theConfigMap = Some(configMap)
  test("one test") {}
  test("two test") {}
  test("red test") {}
  test("blue test", org.scalatest.mytags.FastAsLight) {}
  ignore("ignore me") {}
  class NSuite extends Suite
  override def nestedSuites: List[Suite] = List(new NSuite, new NSuite, new NSuite)
}

object SavesConfigMapSuite {
  private var theConfigMap: Option[Map[String, Any]] = None
  def savedConfigMap = theConfigMap
  def resetConfigMap() { theConfigMap = None }
}
