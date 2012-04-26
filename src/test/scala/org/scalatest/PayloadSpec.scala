package org.scalatest
import org.scalatest.matchers.ShouldMatchers

class PayloadSpec extends FlatSpec with SharedHelpers with ShouldMatchers with Payloads {

  "The modifyPayload method on TFE" should "return the an exception with an equal message option if passed a function that returns the same option passed to it" in {
    val tfe = new TestFailedException("before", 3)
    tfe.modifyPayload(opt => opt) should equal (tfe)
  }
  
  it should "return the new exception with the replaced payload" in {
    val tfe = new TestFailedException("message", 3)
    tfe.modifyPayload(opt => Some("a payload")).payload.get should be ("a payload")
  }
  
  "The withPayload construct" should "allow any non-ModifiablePayload exception to pass through" in {
    val iae = new IllegalArgumentException
    val caught = intercept[IllegalArgumentException] {
      withPayload("howdy") {
        throw iae 
      }
    }
    caught should be theSameInstanceAs (iae)
  }
  
  it should "given a null payload, rethrow the same TFE exception" in {
    val tfe = new TestFailedException("before", 3)
    val caught = intercept[TestFailedException] {
      withPayload(null) {
        throw tfe 
      }
    }
    caught should be theSameInstanceAs (tfe)
  }
  
  it should "given a payload, should throw a new TFE with the given payload" in {
    val tfe = new TestFailedException("message", 3)
    val caught = intercept[TestFailedException] {
      withPayload("a payload") {
        throw tfe 
      }
    }
    caught should not be theSameInstanceAs (tfe)
    caught.payload should be (Some("a payload"))
  }
  
  it should "forward the payload to be carried in TestFailed event" in {
    val a = 
      new Spec {
        it("should do something") {
          withPayload("a payload") {
            throw new TestFailedException("message", 3)
          }
        }
      }
    val rep = new EventRecordingReporter()
    a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker())
    rep.testFailedEventsReceived.length should be (1)
    rep.testFailedEventsReceived(0).payload should be (Some("a payload"))
  }
}