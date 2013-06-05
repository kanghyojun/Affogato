package com.mintpresso.spec

import org.specs2.mutable._
import com.mintpresso._

import scala.collection.mutable.LinkedHashMap

class AffogatoSpec extends Specification {
  val token = "3086671fe6f0-ca84-480b-9cec-0b84fd633ef6"
  val accountId = 3086
  val apiKey = "%s::%d".format(token, accountId)
  val userIdentifier = "admire1"
  val bugsIdentifier = "bugs-identifier1"
  val eitherPointMatcher = (r: Either[Respond, Point]) => r must beRight
  val resultPointMatcher = (r: AffogatoResult) => {
    r.result.asInstanceOf[Either[Respond, Point]] must beRight
  }
  val resultEdgeMatcher = (r: AffogatoResult) => {
    r.result.asInstanceOf[Either[Respond, Edge]] must beRight
  }

  "Mintpresso API Pack" should {
    sequential
    
    "Can init accountId" in {
      val affogato = Affogato(apiKey)

      affogato.accountId === accountId
    }

    "Add a point" in {
      val affogato = Affogato(apiKey)
        
      val res = affogato.set(
        _type="foo", 
        identifier=userIdentifier) 

      eitherPointMatcher(res)
    }


    "Add a point with LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)

      val res: AffogatoResult = affogato.set(LinkedHashMap[String, String](
        "foo" -> userIdentifier
      ))

      resultPointMatcher(res)
    }

    "Add a point with LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      val res = affogato.set(LinkedHashMap[Symbol, String](
        'foo  -> userIdentifier
      ))
      
      resultPointMatcher(res) 
    }

    "Add a point with Point" in {
      val affogato = Affogato(apiKey)

      val res = affogato.set(Point(
        -1,
        "foo",
        userIdentifier,
        """
          {}
        """,
        "",
        0,
        0,
        0
      ))

      eitherPointMatcher(res)
    }

    "Get a Point" in {
      val affogato = Affogato(apiKey)
      val res = affogato.get("foo", userIdentifier)
      eitherPointMatcher(res)
    }

    "Get a point by id" in {
      val affogato = Affogato(apiKey)
      eitherPointMatcher(affogato.get("foo", userIdentifier))
    }

    "Get a point by LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)
      resultPointMatcher(affogato.get(
        LinkedHashMap[String, String]("foo" -> userIdentifier)
      ))
    }

    "Get a point by LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      resultPointMatcher(affogato.get(
        LinkedHashMap[Symbol, String]('foo -> userIdentifier)
      ))
    }
 
    "Add a Edge" in {
      val affogato = Affogato(apiKey)
      affogato.set("foo", userIdentifier)
      affogato.set("barm", bugsIdentifier)

      affogato.set(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier="bugs-identifier1"
      ) must beRight
    }
    
    "Add a Edge by class" in {
      val affogato = Affogato(apiKey)
      var e: Either[Respond, Edge] = null

      for (
        u <- affogato.set("foo", userIdentifier).right;
        m <- affogato.set("barm", bugsIdentifier).right
      ) {
        e = affogato.set(u, verb="listen", m)
      }

      e must beRight
    }

    "Add a Edge with LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)

      val a: AffogatoResult = affogato.set(LinkedHashMap[String, String](
        "foo" -> userIdentifier,
        "do" -> "listen",
        "barm" -> bugsIdentifier
      ))

      resultEdgeMatcher(a)
    }

    "Add a Edge with LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      resultEdgeMatcher(
        affogato.set(LinkedHashMap[Symbol, String](
          'foo -> userIdentifier,
          'do -> "listen",
          'barm -> bugsIdentifier
        ))
      )
    }

    "zGet a Edge" in {
      val affogato = Affogato(apiKey)
      affogato.get(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier
      ) must beRight
    }

    "zGet a Edge without inner point option" in {
      val affogato = Affogato(apiKey)
      affogato.get(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier,
        getInnerPoints=false
      ) must beRight
    }

    "zzGet a Edge by LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)
      val getRes = affogato.get(
        LinkedHashMap[String, String](
          "foo" -> userIdentifier,
          "verb" -> "listen",
          "barm" -> bugsIdentifier
        )
      )

      resultEdgeMatcher(getRes)
    }

    "zzGet a Edge by LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      val getRes = affogato.get(
        LinkedHashMap[Symbol, String](
          'foo -> userIdentifier,
          'verb -> "listen",
          'barm -> bugsIdentifier
        )
      )

      resultEdgeMatcher(getRes)
    }
    
    "zzGet a Edge with ?" in {
      val affogato = Affogato(apiKey)

      val getRes = affogato.get(
        subjectType="foo",
        subjectIdentifier="?",
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier
      )
      getRes must beRight
    }

    "Throw exception when data json is invalid" in {
      val invalidJson = """
      {
        "blah" : 1,
        "daef": 2,
      """
      val affogato = Affogato(apiKey)
      affogato.set(
        _type="foo",
        identifier="bar",
        data=invalidJson
      ) must throwA[AffogatoInvalidJsonException]
    }
  }
}
