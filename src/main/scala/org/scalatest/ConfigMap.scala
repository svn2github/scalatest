/*
 * Copyright 2001-2008 Artima, Inc.
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

import exceptions.TestCanceledException
import reflect.ClassManifest
import collection.immutable.MapLike

class ConfigMap(underlying: Map[String, Any]) extends Map[String, Any] with MapLike[String, Any, ConfigMap] {

  def get(key: String): Option[Any] = underlying.get(key)

  def iterator: Iterator[(String, Any)] = underlying.iterator

  def +[A >: Any](kv: (String, A)): ConfigMap = new ConfigMap(underlying + kv)

  def -(key: String): ConfigMap = new ConfigMap(underlying - key)

  override def empty: ConfigMap = new ConfigMap(Map.empty[String, Any])

  def getOptional[V](key: String)(implicit manifest: ClassManifest[V]): Option[V] = {
    if (underlying.contains(key)) Some(getRequired[V](key))
    else None
  }

  def getWithDefault[V](key: String, default: => V)(implicit manifest: ClassManifest[V]): V = {
    if (underlying.contains(key)) getRequired[V](key)
    else default
  }

  def getRequired[V](key: String)(implicit manifest: ClassManifest[V]): V = {
    underlying.get(key) match {
      case Some(value) =>
        val expectedClass = manifest.erasure
        val boxedExpectedClass =
          expectedClass match {
            case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
            case java.lang.Byte.TYPE => classOf[java.lang.Byte]
            case java.lang.Short.TYPE => classOf[java.lang.Short]
            case java.lang.Integer.TYPE => classOf[java.lang.Integer]
            case java.lang.Long.TYPE => classOf[java.lang.Long]
            case java.lang.Character.TYPE => classOf[java.lang.Character]
            case java.lang.Float.TYPE => classOf[java.lang.Float]
            case java.lang.Double.TYPE => classOf[java.lang.Double]
            case _ => expectedClass
          }
        val actualClass = value.asInstanceOf[AnyRef].getClass
        // if (actualClass.isAssignableFrom(boxedExpectedClass))
        if (boxedExpectedClass.isAssignableFrom(actualClass))
          value.asInstanceOf[V]
        else
            throw new TestCanceledException(Resources("configMapEntryHadUnexpectedType", key, actualClass, expectedClass, value.asInstanceOf[AnyRef]), 1) // TODO: Fix stack depth
      case None => throw new TestCanceledException(Resources("configMapEntryNotFound", key), 1) // TODO: Fix stack depth
    }
  }
}

object ConfigMap {
  def apply(pairs: (String, Any)*): ConfigMap = new ConfigMap(Map(pairs: _*))
  // TODO, create an empty factory method
}
