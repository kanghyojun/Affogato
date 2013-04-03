package com.mintpresso.spec

import org.specs2.mutable._
import com.mintpresso._

class AffogatoSpec extends Specification {
  
  val apiKey = "cc64f8ee51c8420172a907baa81285ae::13"
  
  "Mintpresso API Pack" should {
    
    "Can init accountId" in {
      val affogato = Affogato(apiKey)

      affogato.accountId === 13
    }

    "Add point" in {

      val affogato = Affogato(apiKey)

      affogato.set(_type="user", identifier="admire9@gmail.com") === true
    }

    "Get Point" in {
      val affogato = Affogato(apiKey)
      affogato.get(
        "user",
        "admire9@gmail.com",
        "listen",
        "music",
        "bugs-1"
      ) === true
    }

    "Throw exception when data json is invalid" in {
      val invalidJson = """
      {
        "blah" : 1,
        "daef": 2,
      """
      val affogato = Affogato(apiKey)
      affogato.set(
        _type="user",
        identifier="admire",
        data=invalidJson
      ) must throwA[AffogatoInvalidJsonException]
    }
  }
}
