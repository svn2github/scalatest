/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalautils

import annotation.tailrec

trait XmlEquality {

  import XmlEquality.normalize

  implicit def xmlEquality[T <: xml.Node]: Equality[T] =
    new Equality[T] {
      def areEqual(a: T, b: Any): Boolean = {
        b match {
          case bNode: xml.Node => normalize(a) == normalize(bNode)
          case _ => false
        }
      }
    }
}

object XmlEquality extends XmlEquality {
  private def normalize(node: xml.Node): xml.Node = {
    node match {
      case elem: xml.Elem =>
        val canonicalizedChildren =
          for (child <- node.child if !child.toString.trim.isEmpty) yield {
            child match {
              case elem: xml.Elem => normalize(elem)
              case other => other
            }
          }
        new scala.xml.Elem(elem.prefix, elem.label, elem.attributes, elem.scope, canonicalizedChildren: _*)
      case other => other
    }
  }
}

