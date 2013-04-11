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

    "Add a point as Map[String, String]" in {
      val affogato = Affogato(apiKey)

      affogato.set(Map[String, String](
        "user" -> userIdentifier,
        "name" -> "khs",
        "age" -> "22"
      )) must beSome

    }

    "Add a point as Map[Symbol, String]" in {
      val affogato = Affogato(apiKey)

      affogato.set(Map[Symbol, String](
        'user  -> userIdentifier,
        'name -> "khs",
        'age -> "22"
      )) must beSome

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

    "Get a point by id" in {
      val affogato = Affogato(apiKey)
      val res = affogato.get("user", userIdentifier)
      res.map { p =>
        affogato.get(p.id) must beSome
      }.getOrElse {
        true === false 
      }
    }

    "Add a Edge" in {
      val affogato = Affogato(apiKey)

      affogato.set("user", userIdentifier) must beSome
      affogato.set("music", "bugs-1") must beSome

      val res = affogato.set(
        subjectType="user",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="music",
        objectIdentifier="bugs-1"
      )
      res === true
    }

    "Get a Edge" in {
      val affogato = Affogato(apiKey)

      affogato.set("user", userIdentifier) must beSome
      affogato.set("music", "bugs-1") must beSome

      val res = affogato.set(
        subjectType="user",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="music",
        objectIdentifier="bugs-1"
      )
      res === true

      val getRes = affogato.get(
        subjectType="user",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="music",
        objectIdentifier="bugs-1"
      )
      getRes must beSome
      getRes.map { edges =>
        edges.length must be_>(0)
      }.getOrElse {
        true === false
      }
      val getQueryRes = affogato.get(
        subjectType="user",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="music",
        objectIdentifier="?"
      )

      getQueryRes must beSome
      getQueryRes.map { edges =>
        edges.length must be_>(0)
      }.getOrElse {
        true === false
      }

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
