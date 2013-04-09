/*
 * Copyright 2001-2013 Artima, Inc.
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

import org.scalatest.DocSpec.stripMargin
import org.scalatest.DocSpec.trimMarkup
import Matchers._
import prop.TableDrivenPropertyChecks._
import org.scalatest.SharedHelpers.EventRecordingReporter

class DocSpecSpec extends Spec {
  
  object `A DocSpec` {
    object `with no suites inside` {

      // This one I'm putting flat against the margin on purpose.
      val flatAgainstMargin =
        new DocSpec {
          val doc = markup"""
This is a Title
===============

This is a paragraph later...
"""
        }
      // TODO: Blank line first, line with no chars, line with some white chars, and no lines, and only white lines
      // TODO: test with different end of line characters
      // This one is indented eight characters
      val indented8 =
        new DocSpec {
          val doc = markup"""
            This is a Title
            ===============
            
            This is a paragraph later...
          """
        }

      val emptyLineInMiddle =
        new DocSpec {
          val doc = markup"""
            This is a Title
            ===============

            This is a paragraph later...
          """
        }

      val shortLineInMiddle =
        new DocSpec {
          val doc = markup"""
            This is a Title
            ===============
   
            This is a paragraph later...
          """
        }

      val blankLineFirst =
        new DocSpec {
          val doc = markup"""
            
            This is a Title
            ===============
            
            This is a paragraph later...
          """
        }

      // Initial line has no chars
      val emptyLineFirst =
        new DocSpec {
          val doc = markup"""

            This is a Title
            ===============
            
            This is a paragraph later...
          """
        }

      // Initial line has 3 space chars
      val shortLineFirst =
        new DocSpec {
          val doc = markup"""
   
            This is a Title
            ===============
            
            This is a paragraph later...
          """
        }

      val examples = Table(
        "doc spec",
        flatAgainstMargin,
        indented8,
        emptyLineInMiddle,
        shortLineInMiddle,
        blankLineFirst,
        emptyLineFirst,
        shortLineFirst
      )

      def `should send the markup unindented out the door` {
        forAll (examples) { docSpec =>
          val rep = new EventRecordingReporter
          docSpec.run(None, Args(rep))
          val mp = rep.markupProvidedEventsReceived
          assert(mp.size === 1)
          val event = mp(0)
          val expected =
            trimMarkup("""
              |This is a Title
              |===============
              |
              |This is a paragraph later...
              |""".stripMargin
            )
          assert(event.text === expected)
        }
      }
      def `should return an empty list from nestedSuites` {
        forAll (examples) { doc =>
          doc.nestedSuites should equal (Nil)
        }
      }
    }
    object `with just suites` {
      class NestedSpec extends Spec {
        var wasRun = false
        def `a test` {
          wasRun = true
        }
      }
        
      def `should run the nested suite as well as outputing the markup text` {

        val nestedSpec = new NestedSpec

        val docSpec =
          new DocSpec {
            val doc = markup"""

              This is a Title
              ===============
              
              This is a nested suite: ${
                nestedSpec
              }

              And here is some text after.

          """
        }

        val rep = new EventRecordingReporter
        docSpec.run(None, Args(rep))
        val mp = rep.markupProvidedEventsReceived
        assert(mp.size === 2)
        val expectedTop =
          trimMarkup("""
            |This is a Title
            |===============
            |
            |This is a nested suite: 
          """.stripMargin)
        assert(mp(0).text === expectedTop)

        val expectedBottom = "And here is some text after."
        assert(mp(1).text === expectedBottom)

        assert(nestedSpec.wasRun, "nested spec was not run")
      }
    }
  }

  object `The trimMarkup method` {
    def `should strip any blank lines off of the front` {
     trimMarkup("\n\n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("\n  \n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("  \n  \t \n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("\n\n\n\n  \n  First line with stuff") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff") should equal ("  First line with stuff")
    }
    def `should strip any blank lines off the front and have no blank line or return at the end` {
      trimMarkup("\n\n  First line with stuff\n") should equal ("  First line with stuff")
      trimMarkup("\n  \n  First line with stuff\n\n") should equal ("  First line with stuff")
      trimMarkup("  \n  \t \n  First line with stuff\n  \n") should equal ("  First line with stuff")
      trimMarkup("\n\n\n\n  \n  First line with stuff\n \t\t   \n   \n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n\n\n") should equal ("  First line with stuff")
    }
    def `should have no blank line or return at the end` {
      trimMarkup("  First line with stuff\n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n\n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n  \n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n \t\t   \n   \n") should equal ("  First line with stuff")
      trimMarkup("  First line with stuff\n\n\n") should equal ("  First line with stuff")
    }
  }

  object `The stripMargin method` {
    def `should throw NPE if null passed` {
      evaluating { stripMargin(null) } should produce [NullPointerException] 
    }
    def `should return an empty string as is` {
      stripMargin("") should equal ("")
    }
    def `when passed a string with leading space, should return the string with the leading space omitted` {
      stripMargin(" Howdy") should equal ("Howdy")
      stripMargin("  Howdy") should equal ("Howdy")
      stripMargin("   Howdy") should equal ("Howdy")
      stripMargin("\tHowdy") should equal ("Howdy")
      stripMargin("\t\tHowdy") should equal ("Howdy")
      stripMargin(" \t \tHowdy") should equal ("Howdy")
    }
    def `when passed a string with leading space and two lines, should return the string with the leading space omitted from the first line, and the same amound omitted from the second line, with tabs converted to one space` {
      stripMargin(" Howdy\n123456789") should equal ("Howdy\n23456789")
      stripMargin("  Howdy\n123456789") should equal ("Howdy\n3456789")
      stripMargin("   Howdy\n123456789") should equal ("Howdy\n456789")
      stripMargin("\tHowdy\n123456789") should equal ("Howdy\n23456789")
      stripMargin("\t\tHowdy\n123456789") should equal ("Howdy\n3456789")
      stripMargin(" \t \tHowdy\n123456789") should equal ("Howdy\n56789")
    }
    def `when passed a string with one or more blank lines, a line with leading space and two lines, should return the string with the leading space omitted from the first line, and the same amound omitted from the second line, with tabs converted to one space` {
      stripMargin("\n Howdy\n123456789") should equal ("\nHowdy\n23456789")
      stripMargin("\n  \n\n  Howdy\n123456789") should equal ("\n\n\nHowdy\n3456789")
      stripMargin("\n  \t\t\n   Howdy\n123456789") should equal ("\n\t\nHowdy\n456789")
      stripMargin("\n\n\n\n\tHowdy\n123456789") should equal ("\n\n\n\nHowdy\n23456789")
      stripMargin("\n\t\tHowdy\n123456789") should equal ("\nHowdy\n3456789")
      stripMargin("\n      \n \t \tHowdy\n123456789") should equal ("\n  \nHowdy\n56789")
    }
  }
}
