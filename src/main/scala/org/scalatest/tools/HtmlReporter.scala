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
package org.scalatest.tools

import org.scalatest._
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.util.Iterator
import java.util.Set
import java.io.StringWriter
import org.scalatest.events._
import org.scalatest.exceptions.TestFailedException
import PrintReporter.{BufferSize, makeDurationString}
import HtmlReporter._
import org.pegdown.PegDownProcessor
import scala.collection.mutable.ListBuffer
import scala.xml.NodeSeq
import scala.xml.XML
import java.util.UUID
import scala.xml.Node
import scala.annotation.tailrec
import java.net.URL
import scala.io.Source
import java.nio.channels.Channels
import java.text.DecimalFormat

/**
 * A <code>Reporter</code> that prints test status information in HTML format to a file.
 */
private[scalatest] class HtmlReporter(directoryPath: String, presentAllDurations: Boolean,
        presentInColor: Boolean, presentStackTraces: Boolean, presentFullStackTraces: Boolean, cssUrl: Option[URL]) extends ResourcefulReporter {

  private val directory = new File(directoryPath)
  if (!directory.exists)
    directory.mkdirs()
    
  private def copyResource(url: URL, targetFileName: String) {
    val inputStream = url.openStream
    val outputStream = new FileOutputStream(new File(directory, targetFileName))
    outputStream getChannel() transferFrom(Channels.newChannel(inputStream), 0, Long.MaxValue)
    inputStream.close()
    outputStream.flush()
    outputStream.close()
  }
  
  cssUrl match {
    case Some(cssUrl) => copyResource(cssUrl, "custom.css")
    case None => // Do nothing.
  }
  copyResource(classOf[Suite].getClassLoader.getResource("org/scalatest/HtmlReporter.css"), "styles.css")
  copyResource(classOf[Suite].getClassLoader.getResource("org/scalatest/sorttable.js"), "sorttable.js")
  copyResource(classOf[Suite].getClassLoader.getResource("org/scalatest/d3.v2.min.js"), "d3.v2.min.js")
  
  private val pegDown = new PegDownProcessor

  private def withPossibleLineNumber(stringToPrint: String, throwable: Option[Throwable]): String = {
    throwable match {
      case Some(testFailedException: TestFailedException) =>
        testFailedException.failedCodeFileNameAndLineNumberString match {
          case Some(lineNumberString) =>
            Resources("printedReportPlusLineNumber", stringToPrint, lineNumberString)
          case None => stringToPrint
        }
      case _ => stringToPrint
    }
  }
  
  private def stringsToPrintOnError(noteResourceName: String, errorResourceName: String, message: String, throwable: Option[Throwable],
    formatter: Option[Formatter], suiteName: Option[String], testName: Option[String], duration: Option[Long]): String = {

    formatter match {
      case Some(IndentedText(formattedText, _, _)) =>
        Resources("specTextAndNote", formattedText, Resources(noteResourceName))
      case _ =>
        // Deny MotionToSuppress directives in error events, because error info needs to be seen by users
          suiteName match {
            case Some(sn) =>
              testName match {
                case Some(tn) => Resources(errorResourceName, sn + ": " + tn)
                case None => Resources(errorResourceName, sn)
              }
            // Should not get here with built-in ScalaTest stuff, but custom stuff could get here.
            case None => Resources(errorResourceName, Resources("noNameSpecified"))
          }
      }
  }

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String]): Option[String] =
    stringToPrintWhenNoError(resourceName, formatter, suiteName, testName, None)

  private def stringToPrintWhenNoError(resourceName: String, formatter: Option[Formatter], suiteName: String, testName: Option[String], duration: Option[Long]): Option[String] = {

    formatter match {
      case Some(IndentedText(formattedText, _, _)) =>
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources("withDuration", formattedText, makeDurationString(milliseconds)))
            else
              Some(formattedText)
          case None => Some(formattedText)
        }
      case Some(MotionToSuppress) => None
      case _ =>
        val arg =
          testName match {
            case Some(tn) => suiteName + ": " + tn
            case None => suiteName
          }
        val unformattedText = Resources(resourceName, arg)
        duration match {
          case Some(milliseconds) =>
            if (presentAllDurations)
              Some(Resources("withDuration", unformattedText, makeDurationString(milliseconds)))
            else
              Some(unformattedText)
          case None => Some(unformattedText)
        }

    }
  }
  
  private def getIndentLevel(formatter: Option[Formatter]) = 
    formatter match {
      case Some(IndentedText(formattedText, rawText, indentationLevel)) => indentationLevel
      case _ => 0
  }
  
  private def getSuiteFileName(suiteResult: SuiteResult) = 
    suiteResult.suiteClassName match {
      case Some(suiteClassName) => suiteClassName
      case None => suiteResult.suiteName
    }
  
  private def makeSuiteFile(suiteResult: SuiteResult) {
    val name = getSuiteFileName(suiteResult)
    
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(directory, name + ".html")), BufferSize))
    try {
      pw.println {
        """
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html
  PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
        
        """ + getSuiteHtml(name, suiteResult) 
      }
    }
    finally {
      pw.flush()
      pw.close()
    }
  }
  
  private def appendCombinedStatus(name: String, r: SuiteResult) = 
    if (r.testsFailedCount > 0)
      name + "_with_failed"
    else if (r.testsIgnoredCount > 0 || r.testsPendingCount > 0 || r.testsCanceledCount > 0)
      name + "_passed"
    else
      name + "_passed_all"
  
  private def getSuiteHtml(name: String, suiteResult: SuiteResult) = 
    <html>
      <head>
        <title>ScalaTest Suite { name } Results</title>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <meta http-equiv="Expires" content="-1" />
        <meta http-equiv="Pragma" content="no-cache" />
        <link href="styles.css" rel="stylesheet" />
        { 
          cssUrl match {
            case Some(cssUrl) => 
              <link href="custom.css" rel="stylesheet" />
            case None => NodeSeq.Empty
          }
        }
        <script type="text/javascript">
          { PCDATA("""
            function toggleDetails(contentId, linkId) {
              var ele = document.getElementById(contentId);
              var text = document.getElementById(linkId);
              if(ele.style.display == "block") {
                ele.style.display = "none";
                text.innerHTML = "Show Details";
              }
              else {
                ele.style.display = "block";
                text.innerHTML = "Hide Details";
              }
            }
            """)
          }
        </script>
      </head>
      <body>
        <table id="suite_header">
          <tr id="suite_header_id">
            <td id={ appendCombinedStatus("suite_header_id_label", suiteResult) }>Suite ID</td>
            <td id={ appendCombinedStatus("suite_header_id_value", suiteResult) } colspan="3">{ suiteResult.suiteId }</td>
          </tr>
          <tr id="suite_header_name">
            <td id={ appendCombinedStatus("suite_header_name_label", suiteResult) }>Suite Name</td>
            <td id={ appendCombinedStatus("suite_header_name_value", suiteResult) } colspan="3">{ suiteResult.suiteName }</td>
          </tr>
          <tr id="suite_header_class">
            <td id={ appendCombinedStatus("suite_header_class_label", suiteResult) }>Class Name</td>
            <td id={ appendCombinedStatus("suite_header_class_value", suiteResult) } colspan="3">{ suiteResult.suiteClassName.getOrElse("-") }</td>
          </tr>
        </table>
        {
          val scopeStack = new collection.mutable.Stack[String]()
          suiteResult.eventList.map { e => 
            e match {
              case ScopeOpened(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) => 
                val testNameInfo = nameInfo.testName
                val stringToPrint = stringToPrintWhenNoError("scopeOpened", formatter, nameInfo.suiteName, nameInfo.testName)
                stringToPrint match {
                  case Some(string) => 
                    val elementId = generateElementId
                    scopeStack.push(elementId)
                    scope(elementId, string, getIndentLevel(formatter) + 1)
                  case None => 
                    NodeSeq.Empty
                }
            
              case ScopeClosed(ordinal, message, nameInfo, formatter, location, payload, threadName, timeStamp) =>
                scopeStack.pop
                NodeSeq.Empty
          
              case TestSucceeded(ordinal, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 
            
                val stringToPrint = stringToPrintWhenNoError("testSucceeded", formatter, suiteName, Some(testName), duration)

                val nodeSeq = 
                  stringToPrint match {
                    case Some(string) => 
                      val elementId = generateElementId
                      test(elementId, List(string), getIndentLevel(formatter) + 1, "test_passed")
                    case None =>
                      NodeSeq.Empty
                  }
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case TestFailed(ordinal, message, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, rerunnable, payload, threadName, timeStamp) => 

                val stringToPrint = stringsToPrintOnError("failedNote", "testFailed", message, throwable, formatter, Some(suiteName), Some(testName), duration)
                val elementId = generateElementId
                val nodeSeq = testWithDetails(elementId, List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_failed")            
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case TestIgnored(ordinal, suiteName, suiteID, suiteClassName, testName, testText, formatter, location, payload, threadName, timeStamp) => 

                val stringToPrint =
                  formatter match {
                    case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("ignoredNote")))
                    case Some(MotionToSuppress) => None
                    case _ => Some(Resources("testIgnored", suiteName + ": " + testName))
                  }
 
                stringToPrint match {
                  case Some(string) => 
                    val elementId = generateElementId
                    test(elementId, List(string), getIndentLevel(formatter) + 1, "test_ignored")
                  case None =>
                    NodeSeq.Empty
                }
              
              case TestPending(ordinal, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, duration, formatter, location, payload, threadName, timeStamp) =>

                val stringToPrint =
                  formatter match {
                    case Some(IndentedText(formattedText, _, _)) => Some(Resources("specTextAndNote", formattedText, Resources("pendingNote")))
                    case Some(MotionToSuppress) => None
                    case _ => Some(Resources("testPending", suiteName + ": " + testName))
                  }

                val nodeSeq = 
                  stringToPrint match {
                    case Some(string) => 
                      val elementId = generateElementId
                      test(elementId, List(string), getIndentLevel(formatter) + 1, "test_pending")
                    case None =>
                      NodeSeq.Empty
                  }
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case TestCanceled(ordinal, message, suiteName, suiteID, suiteClassName, testName, testText, recordedEvents, throwable, duration, formatter, location, payload, threadName, timeStamp) =>

                val stringToPrint = stringsToPrintOnError("canceledNote", "testCanceled", message, throwable, formatter, Some(suiteName), Some(testName), duration)
                val elementId = generateElementId
                val nodeSeq = testWithDetails(elementId, List(stringToPrint), message, throwable, getIndentLevel(formatter) + 1, "test_canceled")
            
                nodeSeq :: recordedEvents.map(processInfoMarkupProvided(_)).toList
            
              case infoProvided: InfoProvided =>
                processInfoMarkupProvided(infoProvided)
        
              case markupProvided: MarkupProvided => 
                processInfoMarkupProvided(markupProvided)
                // TO CONTINUE: XML element must be last
            
              case _ => NodeSeq.Empty
            }
          }
        }
      </body>
    </html>
        
  private def processInfoMarkupProvided(event: Event) = {
    event match {
      case InfoProvided(ordinal, message, nameInfo, throwable, formatter, location, payload, threadName, timeStamp) =>
        val (suiteName, testName) =
          nameInfo match {
            case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
            case None => (None, None)
          }
        val infoContent = stringsToPrintOnError("infoProvidedNote", "infoProvided", message, throwable, formatter, suiteName, testName, None)
            
        val elementId = generateElementId
        test(elementId, List(infoContent), getIndentLevel(formatter) + 1, "info")
        
      case MarkupProvided(ordinal, text, nameInfo, formatter, location, payload, threadName, timeStamp) => 
        val (suiteName, testName) =
          nameInfo match {
            case Some(NameInfo(suiteName, _, _, testName)) => (Some(suiteName), testName)
            case None => (None, None)
          }
        
        val elementId = generateElementId
        markup(elementId, text, getIndentLevel(formatter) + 1, "markup")
        
      case _ => NodeSeq.Empty
    }
  }
  
  private def makeIndexFile(resourceName: String, duration: Option[Long]) {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(directory, "index.html")), BufferSize))
    try {
      pw.println {
        """
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html
  PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
        
        """ + getIndexHtml(resourceName, duration) 
      }
    }
    finally {
      pw.flush()
      pw.close()
    }
  }
  
  private def getHeaderStatusColor(summary: Summary) = 
    if (summary.testsFailedCount == 0) "scalatest-header-passed" else "scalatest-header-failed"
  
  private def getRunSummary: Summary = {
    val (succeeded, failed, ignored, pending, canceled) = suiteList.foldLeft((0, 0, 0, 0, 0)) { case ((succeeded, failed, ignored, pending, canceled), r) =>
      (succeeded + r.testsSucceededCount, failed + r.testsFailedCount, ignored + r.testsIgnoredCount, 
       pending + r.testsPendingCount, canceled + r.testsCanceledCount)
    }
    Summary(succeeded, failed, ignored, pending, canceled, suiteList.length, suiteList.filter(!_.isCompleted).length)
  }
  
  private def getPieChartScript(summary: Summary) = {
    import summary._
    
    """
    /* modified from http://www.permadi.com/tutorial/cssGettingBackgroundColor/index.html - */
    function getBgColor(elementId) 
    {
      var element = document.getElementById(elementId);
      if (element.currentStyle)
        return element.currentStyle.backgroundColor;
      if (window.getComputedStyle)
      {
        var elementStyle=window.getComputedStyle(element,"");
        if (elementStyle)
          return elementStyle.getPropertyValue("background-color");
      }
      // Return 0 if both methods failed.  
      return 0;
    }
    
    """ + 
    "var data = [" + testsSucceededCount + ", " + testsFailedCount + ", " + testsIgnoredCount + ", " + testsPendingCount + ", " + testsCanceledCount + "];" + 
    "var color = [getBgColor('summary_view_row_1_legend_succeeded_label'), " + 
    "             getBgColor('summary_view_row_1_legend_failed_label'), " + 
    "             getBgColor('summary_view_row_1_legend_ignored_label'), " + 
    "             getBgColor('summary_view_row_1_legend_pending_label'), " +
    "             getBgColor('summary_view_row_1_legend_canceled_label')" + 
    "            ];" + 
    """
    var width = document.getElementById('chart_div').offsetWidth,
        height = document.getElementById('chart_div').offsetHeight,
        outerRadius = Math.min(width, height) / 2,
        innerRadius = 0,
        donut = d3.layout.pie(),
        arc = d3.svg.arc().innerRadius(innerRadius).outerRadius(outerRadius);
    
    var vis = d3.select("#chart_div")
                .append("svg")
                .data([data])
                .attr("width", width)
                .attr("height", height);
    
    var arcs = vis.selectAll("g.arc")
                  .data(donut)
                  .enter().append("g")
                  .attr("class", "arc")
                  .attr("transform", "translate(" + outerRadius + "," + outerRadius + ")");
    
    arcs.append("path")
        .attr("fill", function(d, i) { return color[i]; })
        .attr("d", arc);
    """
  }
  
  private def getIndexHtml(resourceName: String, duration: Option[Long]) = {
    val summary = getRunSummary
    import summary._

    val decimalFormat = new DecimalFormat("#.##")
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
      <head>
        <title>ScalaTest Results</title>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <meta http-equiv="Expires" content="-1" />
        <meta http-equiv="Pragma" content="no-cache" />
        <link href="styles.css" rel="stylesheet" />
        { 
          cssUrl match {
            case Some(cssUrl) => 
              <link href="custom.css" rel="stylesheet" />
            case None => NodeSeq.Empty
          }
        }
        <script type="text/javascript" src="d3.v2.min.js"></script>
        <script type="text/javascript" src="sorttable.js"></script>
        <script type="text/javascript">
          { PCDATA("""
            var tagMap = {};    
            
            var SUCCEEDED_BIT = 1;
            var FAILED_BIT = 2;
            var IGNORED_BIT = 4;
            var PENDING_BIT = 8;
            var CANCELED_BIT = 16;
              
            function applyFilter() {
              var mask = 0;
              if (document.getElementById('succeeded_checkbox').checked)
                mask |= SUCCEEDED_BIT; 
              if (document.getElementById('failed_checkbox').checked)
                mask |= FAILED_BIT;
              if (document.getElementById('ignored_checkbox').checked)
                mask |= IGNORED_BIT;
              if (document.getElementById('pending_checkbox').checked)
                mask |= PENDING_BIT;
              if (document.getElementById('canceled_checkbox').checked)
                mask |= CANCELED_BIT;

              for (var key in tagMap) {
                if (tagMap.hasOwnProperty(key)) {
                  var bitSet = tagMap[key];
                  var element = document.getElementById(key);
                  if ((bitSet & mask) != 0) 
                    element.style.display = "table-row";
                  else 
                    element.style.display = "none";
                }
              }
            }
              
            function showDetails(suiteName) {
              document.getElementById('details_view').innerHTML = "<iframe src='" + suiteName + ".html' width='100%' height='100%'></iframe>";
            }
              
            function resizeDetailsView() {
              var headerView = document.getElementById('scalatest-header');
              var detailsView = document.getElementById('details_view');
              var summaryView = document.getElementById('summary_view');
              var left = summaryView.offsetWidth + 30;
              detailsView.style.left = left + "px";
              detailsView.style.width = (window.innerWidth - left - 30) + "px";
              detailsView.style.height = (window.innerHeight - headerView.offsetHeight - 20) + "px";
            }
          """) }
        </script>
      </head>
      <body onresize="resizeDetailsView()">
        <div class="scalatest-report"> 
          { header(resourceName, duration, summary) }
          <table id="summary_view">
            <tr id="summary_view_row_1">
              <td id="summary_view_row_1_chart">
                <div id="chart_div"></div>
              </td>
              <td id="summary_view_row_1_legend">
                <table id="summary_view_row_1_legend_table">
                  <tr id="summary_view_row_1_legend_table_row_succeeded">
                    <td id="summary_view_row_1_legend_succeeded_label">Succeeded</td>
                    <td id="summary_view_row_1_legend_succeeded_count">{ testsSucceededCount }</td>
                    <td id="summary_view_row_1_legend_succeeded_percent">({ decimalFormat.format(testsSucceededCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_failed">
                    <td id="summary_view_row_1_legend_failed_label">Failed</td>
                    <td id="summary_view_row_1_legend_failed_count">{ testsFailedCount }</td>
                    <td id="summary_view_row_1_legend_failed_percent">({ decimalFormat.format(testsFailedCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_ignored">
                    <td id="summary_view_row_1_legend_ignored_label">Ignored</td>
                    <td id="summary_view_row_1_legend_ignored_count">{ testsIgnoredCount }</td>
                    <td id="summary_view_row_1_legend_ignored_percent">({ decimalFormat.format(testsIgnoredCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_pending">
                    <td id="summary_view_row_1_legend_pending_label">Pending</td>
                    <td id="summary_view_row_1_legend_pending_count">{ testsPendingCount }</td>
                    <td id="summary_view_row_1_legend_pending_percent">({ decimalFormat.format(testsPendingCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                  <tr id="summary_view_row_1_legend_table_row_canceled">
                    <td id="summary_view_row_1_legend_canceled_label">Canceled</td>
                    <td id="summary_view_row_1_legend_canceled_count">{ testsCanceledCount }</td>
                    <td id="summary_view_row_1_legend_canceled_percent">({ decimalFormat.format(testsCanceledCount * 100.0 / totalTestsCount) }%)</td>
                  </tr>
                </table>
              </td>
            </tr>
            <tr id="summary_view_row_2">
              <td id="summary_view_row_2_results" colspan="2">{ suiteResults }</td>
            </tr>
          </table>
          <div id="details_view">
            Click on suite name to view details.
          </div>
        </div>
        <script type="text/javascript">
          { PCDATA(getPieChartScript(summary)) }
        </script>
        <script type="text/javascript">
          { PCDATA(tagMapScript) }
        </script>
        <script type="text/javascript">
          { PCDATA("""
              resizeDetailsView();
            """)
          }
        </script>
      </body>
    </html>
  }      
          
  private def getStatistic(summary: Summary) = 
    <div id="display-filters">
      <input id="succeeded_checkbox" name="succeeded_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="passed_checkbox">Succeeded: { summary.testsSucceededCount }</label>
      <input id="failed_checkbox" name="failed_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="failed_checkbox">Failed: { summary.testsFailedCount }</label>
      <input id="ignored_checkbox" name="ignored_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="ignored_checkbox">Ignored: { summary.testsIgnoredCount }</label>
      <input id="pending_checkbox" name="pending_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="pending_checkbox">Pending: { summary.testsPendingCount }</label>
      <input id="canceled_checkbox" name="canceled_checkbox" type="checkbox" checked="checked" onchange="applyFilter()" /> <label for="canceled_checkbox">Canceled: { summary.testsCanceledCount }</label>
    </div>
  
  private def header(resourceName: String, duration: Option[Long], summary: Summary) = 
    <div id="scalatest-header" class={ getHeaderStatusColor(summary) }>
      <div id="title">
        ScalaTest Results
        { getStatistic(summary) }
      </div>

      <div id="summary">
        <p id="duration">{ getDuration(resourceName, duration) }</p>    
        <p id="totalTests">{ getTotalTests(summary) }</p>
        <p id="suiteSummary">{ getSuiteSummary(summary) }</p>
      </div>
    </div>
        
  private def generateElementId = UUID.randomUUID.toString
  
  private def setBit(stack: collection.mutable.Stack[String], tagMap: collection.mutable.HashMap[String, Int], bit: Int) {
    stack.foreach { scopeElementId => 
      val currentBits = tagMap(scopeElementId)
      tagMap.put(scopeElementId, currentBits | bit)
    }
  }
  
  val tagMap = collection.mutable.HashMap[String, Int]()
        
  private def suiteResults = 
    <table class="sortable">
      <tr>
        <td>Suite</td>
        <td>Duration (ms.)</td>
        <td>Succeeded</td>
        <td>Failed</td>
        <td>Ignored</td>
        <td>Pending</td>
        <td>Canceled</td>
        <td>Total</td>
      </tr>
    {
      val sortedSuiteList = suiteList.sortWith { (a, b) => 
        if (a.testsFailedCount == b.testsFailedCount)
          a.startEvent.suiteName < b.startEvent.suiteName
        else
          a.testsFailedCount > b.testsFailedCount
      }.toArray
      sortedSuiteList map { r =>
        val elementId = generateElementId
        import r._
        val bits = 
          (if (testsSucceededCount > 0) SUCCEEDED_BIT else 0) +
          (if (testsFailedCount > 0) FAILED_BIT else 0) +
          (if (testsIgnoredCount > 0) IGNORED_BIT else 0) + 
          (if (testsPendingCount > 0) PENDING_BIT else 0) + 
          (if (testsCanceledCount > 0) CANCELED_BIT else 0)
        tagMap.put(elementId, bits)
        suiteSummary(elementId,  getSuiteFileName(r), r)
      }
    }
    </table>
      
  private def countStyle(prefix: String, count: Int) = 
    if (count == 0)
      prefix + "_zero"
    else
      prefix
      
  private def durationDisplay(duration: Option[Long]) = 
    duration match {
      case Some(duration) => duration
      case None => "-"
    }
    
  private def suiteSummary(elementId: String, suiteFileName: String, suiteResult: SuiteResult) = {
    import suiteResult._
    <tr id={ elementId }>
      <td class={ appendCombinedStatus("suite_name", suiteResult) }><a href={ "javascript: showDetails('" + suiteFileName + "')" }>{ suiteName }</a></td>
      <td class={ appendCombinedStatus("duration", suiteResult) }>{ durationDisplay(duration) }</td>
      <td class={ countStyle("succeeded", testsSucceededCount) }>{ testsSucceededCount }</td>
      <td class={ countStyle("failed", testsFailedCount) }>{ testsFailedCount }</td>
      <td class={ countStyle("ignored", testsIgnoredCount) }>{ testsIgnoredCount }</td>
      <td class={ countStyle("pending", testsPendingCount) }>{ testsPendingCount }</td>
      <td class={ countStyle("canceled", testsCanceledCount) }>{ testsCanceledCount }</td>
      <td class={ appendCombinedStatus("total", suiteResult) }>{ testsSucceededCount + testsFailedCount + testsIgnoredCount + testsPendingCount + testsCanceledCount }</td>
    </tr>
  }
        
  private def suite(elementId: String, suiteName: String, indentLevel: Int) = 
    <div id={ elementId } class="suite" style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        <dt>{ suiteName }</dt>
      </dl>
    </div>
        
  private def scope(elementId: String, message: String, indentLevel: Int) = 
    <div id={ elementId } class="scope" style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      { message }
    </div>
      
  private def test(elementId: String, lines: List[String], indentLevel: Int, styleName: String) = 
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        {
          lines.map { line => 
            <dt>{ line }</dt>
          }
        }
      </dl>
    </div>
  
  private def testWithDetails(elementId: String, lines: List[String], message: String, throwable: Option[Throwable], indentLevel: Int, styleName: String) = {
    def getHTMLForStackTrace(stackTraceList: List[StackTraceElement]) =
              stackTraceList.map((ste: StackTraceElement) => <span>{ ste.toString }</span><br />)
    
    def getHTMLForCause(throwable: Throwable): scala.xml.NodeBuffer = {
      val cause = throwable.getCause
      if (cause != null) {
        <table>
          <tr valign="top">
            <td align="right"><span class="label">{ Resources("DetailsCause") + ":" }</span></td>
            <td align="left">{ cause.getClass.getName }</td>
          </tr>
          <tr valign="top">
            <td align="right"><span class="label">{ Resources("DetailsMessage") + ":" }</span></td>
            <td align="left"><span>{ if (cause.getMessage != null) cause.getMessage else Resources("None") }</span></td>
          </tr>
        </table>
        <table>
          <tr valign="top">
            <td align="left" colspan="2">{ getHTMLForStackTrace(cause.getStackTrace.toList) }</td>
          </tr>
        </table> &+ getHTMLForCause(cause)
      }
      else new scala.xml.NodeBuffer
    }
    
    val (grayStackTraceElements, blackStackTraceElements) =
      throwable match {
        case Some(throwable) =>
          val stackTraceElements = throwable.getStackTrace.toList
          throwable match {
            case tfe: TestFailedException =>
              (stackTraceElements.take(tfe.failedCodeStackDepth), stackTraceElements.drop(tfe.failedCodeStackDepth))
            case _ => (List(), stackTraceElements)
          } 
        case None => (List(), List())
      }
    
    val throwableTitle = 
      throwable match {
        case Some(throwable) => Some(throwable.getClass.getName)
        case None => None
      }
    
    val fileAndLineOption: Option[String] = 
      throwable match {
        case Some(throwable) =>
          throwable match {
            case stackDepth: StackDepth =>
              stackDepth.failedCodeFileNameAndLineNumberString
            case _ => None
          }
        case None => None
      }
    
    val linkId = UUID.randomUUID.toString
    val contentId = UUID.randomUUID.toString
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
      <dl>
        {
          lines.map { line => 
            <dt>{ line }</dt>
          }
        }
      </dl>
      <a id={ linkId } href={ "javascript:toggleDetails('" + contentId + "', '" + linkId + "');" }>Show Details</a>
      <div id={ contentId } style="display: none">
        <table>
          {
            //if (mainMessage.isDefined) {
              <tr valign="top"><td align="left"><span class="label">{ Resources("DetailsMessage") + ":" }</span></td><td align="left">
                <span class="dark">
                { 
                  // Put <br>'s in for line returns at least, so property check failure messages look better
                  val messageLines = message.split("\n")
                  if (messageLines.size > 1)
                    messageLines.map(line => <span>{ line }<br/></span>)
                  else
                    <span>{ message }</span>
                }
                </span>
              </td></tr>
            //}
            //else <!-- -->
          }
          {
            fileAndLineOption match {
              case Some(fileAndLine) =>
                <tr valign="top"><td align="left"><span class="label">{ Resources("LineNumber") + ":" }</span></td><td align="left"><span class="dark">{ "(" + fileAndLine + ")" }</span></td></tr>
              case None =>
            }
          }
          {
            throwableTitle match {
              case Some(title) =>
                <tr valign="top"><td align="right"><span class="label">{ Resources("DetailsThrowable") + ":" }</span></td><td align="left">{ title }</td></tr>
              case None => new scala.xml.NodeBuffer
            }
          }
        </table>
        <table>
          <tr valign="top"><td align="left" colspan="2">
            { grayStackTraceElements.map((ste: StackTraceElement) => <span class="gray">{ ste.toString }</span><br />) }
            { blackStackTraceElements.map((ste: StackTraceElement) => <span class="dark">{ ste.toString }</span><br />) }
            </td>
          </tr>
        </table>
        {
          throwable match {
            case Some(t) => getHTMLForCause(t)
            case None =>
          }
        }
      </div>
    </div>
  }
        
  private def markup(elementId: String, text: String, indentLevel: Int, styleName: String) = 
    <div id={ elementId } class={ styleName } style={ "margin-left: " + (20 * indentLevel) + "px;" }>
       { XML.loadString(pegDown.markdownToHtml(text)) }
    </div>
       
  private def tagMapScript = 
    "tagMap = { \n" + 
      tagMap.map { case (elementId, bitSet) => "\"" + elementId + "\": " + bitSet }.mkString(", \n") + 
    "};\n" + 
    "applyFilter();"

  case class SuiteResult(suiteId: String, suiteName: String, suiteClassName: Option[String], duration: Option[Long], startEvent: SuiteStarting, endEvent: Event, eventList: IndexedSeq[Event], 
                          testsSucceededCount: Int, testsFailedCount: Int, testsIgnoredCount: Int, testsPendingCount: Int, testsCanceledCount: Int, isCompleted: Boolean)
    
  private var eventList = new ListBuffer[Event]()
  private val suiteList = new ListBuffer[SuiteResult]()
  private var runEndEvent: Option[Event] = None
        
  def apply(event: Event) {
        
    event match {

      case RunStarting(ordinal, testCount, configMap, formatter, location, payload, threadName, timeStamp) => 

      case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 
        runEndEvent = Some(event)

      case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
        runEndEvent = Some(event)

      case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
        runEndEvent = Some(event)
        
      case SuiteCompleted(ordinal, suiteName, suiteId, suiteClassName, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
        val (suiteEvents, otherEvents) = extractSuiteEvents(suiteId)
        eventList = otherEvents
        val sortedSuiteEvents = suiteEvents.sorted
        sortedSuiteEvents.head match {
          case suiteStarting: SuiteStarting => 
            val suiteResult = sortedSuiteEvents.foldLeft(SuiteResult(suiteId, suiteName, suiteClassName, duration, suiteStarting, event, sortedSuiteEvents.tail.toIndexedSeq, 0, 0, 0, 0, 0, true)) { case (r, e) => 
              e match {
                case testSucceeded: TestSucceeded => r.copy(testsSucceededCount = r.testsSucceededCount + 1)
                case testFailed: TestFailed => r.copy(testsFailedCount = r.testsFailedCount + 1)
                case testIgnored: TestIgnored => r.copy(testsIgnoredCount = r.testsIgnoredCount + 1)
                case testPending: TestPending => r.copy(testsPendingCount = r.testsPendingCount + 1)
                case testCanceled: TestCanceled => r.copy(testsCanceledCount = r.testsCanceledCount + 1)
                case _ => r
              }
            }
            suiteList += suiteResult
            makeSuiteFile(suiteResult)
          case other => 
            throw new IllegalStateException("Expected SuiteStarting in the head of suite events, but we got: " + other.getClass.getName)
        }
            
      case SuiteAborted(ordinal, message, suiteName, suiteId, suiteClassName, throwable, duration, formatter, location, rerunner, payload, threadName, timeStamp) =>
        val (suiteEvents, otherEvents) = extractSuiteEvents(suiteId)
        eventList = otherEvents
        val sortedSuiteEvents = suiteEvents.sorted
        sortedSuiteEvents.head match {
          case suiteStarting: SuiteStarting => 
            val suiteResult = sortedSuiteEvents.foldLeft(SuiteResult(suiteId, suiteName, suiteClassName, duration, suiteStarting, event, sortedSuiteEvents.tail.toIndexedSeq, 0, 0, 0, 0, 0, false)) { case (r, e) => 
              e match {
                case testSucceeded: TestSucceeded => r.copy(testsSucceededCount = r.testsSucceededCount + 1)
                case testFailed: TestFailed => r.copy(testsFailedCount = r.testsFailedCount + 1)
                case testIgnored: TestIgnored => r.copy(testsIgnoredCount = r.testsIgnoredCount + 1)
                case testPending: TestPending => r.copy(testsPendingCount = r.testsPendingCount + 1)
                case testCanceled: TestCanceled => r.copy(testsCanceledCount = r.testsCanceledCount + 1)
                case _ => r
              }
            }
            suiteList += suiteResult
            makeSuiteFile(suiteResult)
          case other => 
            throw new IllegalStateException("Expected SuiteStarting in the head of suite events, but we got: " + other.getClass.getName)
        }
      
      case _ => eventList += event
    }
  }
      
  def extractSuiteEvents(suiteId: String) = eventList partition { e => 
    e match {
      case e: TestStarting => e.suiteId == suiteId
      case e: TestSucceeded  => e.suiteId == suiteId
      case e: TestIgnored    => e.suiteId == suiteId
      case e: TestFailed     => e.suiteId == suiteId
      case e: TestPending    => e.suiteId == suiteId
      case e: TestCanceled   => e.suiteId == suiteId
      case e: InfoProvided   => 
        e.nameInfo match {
          case Some(nameInfo) => 
            nameInfo.suiteID == suiteId
          case None => false
        }
      case e: MarkupProvided => 
        e.nameInfo match {
          case Some(nameInfo) => 
            nameInfo.suiteID == suiteId
          case None => false
        }
      case e: ScopeOpened    => e.nameInfo.suiteID == suiteId
      case e: ScopeClosed    => e.nameInfo.suiteID == suiteId
      case e: SuiteStarting  => e.suiteId == suiteId
      case _ => false
    }
  }
      
  def dispose() {
    runEndEvent match {
      case Some(event) => 
        event match {
          case RunCompleted(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) => 
            makeIndexFile("runCompleted", duration)

          case RunStopped(ordinal, duration, summary, formatter, location, payload, threadName, timeStamp) =>
            makeIndexFile("runStopped", duration)

          case RunAborted(ordinal, message, throwable, duration, summary, formatter, location, payload, threadName, timeStamp) => 
            makeIndexFile("runAborted", duration)
            
          case other =>
            throw new IllegalStateException("Expected run ending event only, but got: " + other.getClass.getName)
        }
      case None => // If no run end event (e.g. when run in sbt), just use runCompleted
        makeIndexFile("runCompleted", None)
    }
    
  }
  
  private def getDuration(resourceName: String, duration: Option[Long]) = {
    duration match {
      case Some(msSinceEpoch) =>
        Resources(resourceName + "In", makeDurationString(msSinceEpoch))
      case None =>
        Resources(resourceName)
    }
  }
  
  private def getTotalTests(summary: Summary) = 
    Resources("totalNumberOfTestsRun", summary.testsCompletedCount.toString)    
    
  private def getSuiteSummary(summary: Summary) = 
    Resources("suiteSummary", summary.suitesCompletedCount.toString, summary.suitesAbortedCount.toString)

  // We subtract one from test reports because we add "- " in front, so if one is actually zero, it will come here as -1
  // private def indent(s: String, times: Int) = if (times <= 0) s else ("  " * times) + s

  // Stupid properties file won't let me put spaces at the beginning of a property
  // "  {0}" comes out as "{0}", so I can't do indenting in a localizable way. For now
  // just indent two space to the left.  //  if (times <= 0) s 
  //  else Resources("indentOnce", indent(s, times - 1)) 
}

private[tools] object HtmlReporter {  
  final val SUCCEEDED_BIT = 1
  final val FAILED_BIT = 2
  final val IGNORED_BIT = 4
  final val PENDING_BIT = 8
  final val CANCELED_BIT = 16
}

private[tools] object PCDATA {
  def apply(in: String): Node = scala.xml.Unparsed(in)
}