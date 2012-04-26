package org.scalatest

trait Payloads {

  def withPayload(payload: => Any)(fun: => Unit) {
    try {
      fun
    }
    catch {
      case e: ModifiablePayload[_] =>
        if (payload != null)
          throw e.modifyPayload((currentPayload: Option[Any]) => Some(payload))
        else
          throw e
    }
  }
  
}