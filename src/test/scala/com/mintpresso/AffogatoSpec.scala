package com.mintpresso.spec

import org.specs2.mutable._
import com.mintpresso._

import scala.collection.mutable.LinkedHashMap

class AffogatoSpec extends Specification {
  val token = "3086671fe6f0-ca84-480b-9cec-0b84fd633ef6"
  val accountId = 3086
  val apiKey = "%s::%d".format(token, accountId)
  val userIdentifier = "admire93"

  val eitherPointMatcher = (r: Either[Respond, Point]) => r must beRight
  val resultPointMatcher = (r: AffogatoResult) => r.fold(
    e => false === true,
    (p: Point) => p.identifier === userIdentifier
  )
  val resultEdgeMatcher = (r: AffogatoResult) => r.fold(
    e => false === true,
    (e: Edge) => true === true 
  )

  "Mintpresso API Pack" should {
    
    "Can init accountId" in {
      val affogato = Affogato(apiKey)

      affogato.accountId === accountId
    }

    "Add a point" in {

      val affogato = Affogato(apiKey)
        
      val res = affogato.set(
        _type="tuser", 
        identifier=userIdentifier, 
        data="""{
          "name": "khs",
          "age": "22"
        }""")

      eitherPointMatcher(res)
    }


    "Add a point with LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)

      val res: AffogatoResult = affogato.set(LinkedHashMap[String, String](
        "tuser" -> userIdentifier,
        "name" -> "khs",
        "age" -> "22"
      ))

      resultPointMatcher(res)
    }

    "Add a point with LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      val res = affogato.set(LinkedHashMap[Symbol, String](
        'tuser  -> userIdentifier,
        'name -> "khs",
        'age -> "22"
      ))
      
      resultPointMatcher(res) 
    }

    "Add a point with Point" in {
      val affogato = Affogato(apiKey)

      val res = affogato.set(Point(
        -1,
        "tuser",
        userIdentifier,
        """{
          "name": "khs",
          "age": "22"
        }""",
        "",
        0,
        0,
        0
      ))

      eitherPointMatcher(res)
    }

    "Get a Point" in {
      val affogato = Affogato(apiKey)
      val res = affogato.get("tuser", userIdentifier)
      eitherPointMatcher(res)
    }


    "Get a point by id" in {
      val affogato = Affogato(apiKey)
      eitherPointMatcher(affogato.get("tuser", userIdentifier))
    }
    
    "Add a Edge" in {
      val affogato = Affogato(apiKey)

      affogato.set("tuser", userIdentifier)
      affogato.set("music", "bugs-1")

      affogato.set(
        subjectType="tuser",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="music",
        objectIdentifier="bugs-1"
      ) must beRight
    }
    
    "Add a Edge by class" in {
      val affogato = Affogato(apiKey)
      var e: Either[Respond, Edge] = null

      for (
        u <- affogato.set("tuser", userIdentifier).right;
        m <- affogato.set("music", "bugs-1").right
      ) {
        e = affogato.set(u, verb="listen", m)
      }

      e must beRight
    }

    "Add a Edge with LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)

      resultEdgeMatcher(
        affogato.set(LinkedHashMap[String, String](
          "tuser" -> userIdentifier,
          "do" -> "listen",
          "music" -> "bugs-1"
        ))
      )
    }

    "Add a Edge with LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      resultEdgeMatcher(
        affogato.set(LinkedHashMap[Symbol, String](
          'tuser -> userIdentifier,
          'do -> "listen",
          'music -> "bugs-1"
        ))
      )
    }

    /*
    

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
    
    "Get a Edge with ?" in {
      val affogato = Affogato(apiKey)

      val getRes = affogato.get(
        subjectType="user",
        subjectIdentifier="?",
        verb="listen",
        objectType="music",
        objectIdentifier="bugs-1"
      )
      getRes must beSome[List[Edge]].which { edges =>
        edges.length must be_>(0)
      }
      val getQueryRes = affogato.get(
        subjectType="user",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="music",
        objectIdentifier="?"
      )

      getQueryRes must beSome[List[Edge]].which { edges =>
        edges.length must be_>(0)
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
  */
  }
}
