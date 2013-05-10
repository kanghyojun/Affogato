package com.mintpresso.spec

import org.specs2.mutable._
import com.mintpresso._

class AffogatoSpec extends Specification {
  
  val token = "240ff06dee7-f79f-423f-9684-0cedd2c13ef3"
  val accountId = 240
  val apiKey = "%s::%d".format(token, accountId)
  val userIdentifier = "admire93"
  
  "Mintpresso API Pack" should {
    
    "Can init accountId" in {
      val affogato = Affogato(apiKey)

      affogato.accountId === accountId
    }

    "Add a point" in {

      val affogato = Affogato(apiKey)

      affogato.set(
        _type="user", 
        identifier=userIdentifier, 
        data="""{
          "name": "khs",
          "age": "22"
        }""") must beSome
    }

    "Add a point with Map[String, String]" in {
      val affogato = Affogato(apiKey)

      affogato.set(Map[String, String](
        "user" -> userIdentifier,
        "name" -> "khs",
        "age" -> "22"
      )).as[Option[Point]] must beSome

    }

    "Add a point with Map[Symbol, String]" in {
      val affogato = Affogato(apiKey)

      affogato.set(Map[Symbol, String](
        'user  -> userIdentifier,
        'name -> "khs",
        'age -> "22"
      )).as[Option[Point]] must beSome
    }

    "Add a point with Point" in {
      val affogato = Affogato(apiKey)

      affogato.set(Point(
        -1,
        "user",
        userIdentifier,
        """{
          "name": "khs",
          "age": "22"
        }""",
        ""
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
      affogato.get("user", userIdentifier).as[Option[Point]] must beSome[Point].which { p =>
        p.identifier === userIdentifier
      }
    }

    "Get a point by id" in {
      val affogato = Affogato(apiKey)
      val res = affogato.get("user", userIdentifier).as[Option[Point]]
      res.map { p =>
        affogato.get(p.id) must beSome
      }.getOrElse {
        true === false 
      }
    }

    "Get a point by type" in {
      val affogato = Affogato(apiKey)
      true === true
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

    "Add a Edge with Map[String, String]" in {
      val affogato = Affogato(apiKey)

      affogato.set("user", userIdentifier) must beSome
      affogato.set("music", "bugs-1") must beSome

      val res = affogato.set(Map[String, String](
        "user" -> userIdentifier,
        "do" -> "listen",
        "music" -> "bugs-1"
      )).as[Boolean]

      res === true
    }

    "Add a Edge with Map[Symbol, String]" in {
      val affogato = Affogato(apiKey)

      affogato.set("user", userIdentifier) must beSome
      affogato.set("music", "bugs-1") must beSome

      val res = affogato.set(Map[Symbol, String](
        'user -> userIdentifier,
        'do -> "listen",
        'music -> "bugs-1"
      )).as[Boolean]

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
