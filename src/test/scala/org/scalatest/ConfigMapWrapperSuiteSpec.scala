package org.scalatest

class ConfigMapWrapperSuiteSpec extends FunSuite with SharedHelpers with SeveredStackTraces {

  // Need a test that ensures the passed config map gets in there.
  test("configMap should get passed into the wrapped Suite") {
    SavesConfigMapSuite.resetConfigMap()
    val wrapped = new ConfigMapWrapperSuite(classOf[SavesConfigMapSuite])
    val configMap = Map("salt" -> "pepper", "eggs" -> "bacon")
    wrapped.run(None, SilentReporter, new Stopper {}, Filter(), configMap, None, new Tracker)
    assert(SavesConfigMapSuite.savedConfigMap === Some(configMap))
  }
  
  test("configMap's expected test count should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.expectedTestCount(Filter(None, Set())) === 4)
    assert(suite.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 3)
  }

  test("configMap's testNames should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.testNames.toList === List("one test", "two test", "red test", "blue test", "ignore me"))
  }

  test("configMap's nestedSuites should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.nestedSuites.size === 3)
  }

  test("configMap's tags method should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.tags === Map("blue test" -> Set("org.scalatest.FastAsLight"), "ignore me" -> Set("org.scalatest.Ignore")))
  }
}
