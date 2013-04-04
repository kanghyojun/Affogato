package com.mintpresso.spec

import org.specs2.mutable._
import com.mintpresso._

class AffogatoSpec extends Specification {
  
  val apiKey = "cc64f8ee51c8420172a907baa81285ae::13"
  val userIdentifier = "admire9@gmail.com"
  
  "Mintpresso API Pack" should {
    
    "Can init accountId" in {
      val affogato = Affogato(apiKey)

      affogato.accountId === 13
    }

    "Add a point" in {

      val affogato = Affogato(apiKey)

      affogato.set(
        _type="user", 
        identifier=userIdentifier, 
        data="""{
          "name": "khs",
          "age": "22"
        }""").isEmpty === false
    }

    "Return a Point when add a point" in {
      val affogato = Affogato(apiKey)
      affogato.set(
        _type="user", 
        identifier=userIdentifier
      ).map { point =>
        (point._type, point.identifier)
      } === Some(("user", userIdentifier))
    }

    "Get a Point" in {
      val affogato = Affogato(apiKey)
      val res = affogato.get("user", userIdentifier)
      res.map { p =>
        p.identifier === userIdentifier
      }.getOrElse {
        res.isEmpty === false
      }
    }

    "Get a Edge" in {
      val affogato = Affogato(apiKey)
      affogato.get(
        "user",
        userIdentifier,
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
