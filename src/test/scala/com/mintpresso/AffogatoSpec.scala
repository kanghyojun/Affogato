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
  val eitherPointsMatcher = (r: Either[Respond, Points]) => r must beRight
  val resultPointMatcher = (r: AffogatoResult) => {
    r.result.asInstanceOf[Either[Respond, Points]] must beRight
  }
  val resultEdgeMatcher = (r: AffogatoResult) => {
    r.result.asInstanceOf[Either[Respond, Edge]] must beRight
  }
  val resultListEdgeMatcher = (r: AffogatoResult) => {
    r.result.asInstanceOf[Either[Respond, Edges]] match {
      case Left(a) => println(a.message)
      case Right(_) => println("")
    }
    r.result.asInstanceOf[Either[Respond, Edges]] must beRight
  }

  "Mintpresso API Pack" should {
    sequential
    
    "Can init accountId" in {
      val affogato = Affogato(apiKey)

      affogato.accountId === accountId
    }
  }

  "Point" can {
    sequential

    "be added" in {
      val affogato = Affogato(apiKey)
        
      val res = affogato.set(
        _type="foo", 
        identifier=userIdentifier) 

      eitherPointMatcher(res)
    }

    "be added with LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)

      val res: AffogatoResult = affogato.set(LinkedHashMap[String, String](
        "foo" -> userIdentifier
      ))

      resultPointMatcher(res)
    }

    "be added with LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      val res = affogato.set(LinkedHashMap[Symbol, String](
        'foo  -> userIdentifier
      ))
      
      resultPointMatcher(res) 
    }

    "be added with Point" in {
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

    "be found" in {
      val affogato = Affogato(apiKey)
      val res = affogato.get("foo", userIdentifier)
      eitherPointsMatcher(res)
    }

    "be found by LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)
      resultPointMatcher(affogato.get(
        LinkedHashMap[String, String]("foo" -> userIdentifier)
      ))
    }

    "be found by LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      resultPointMatcher(affogato.get(
        LinkedHashMap[Symbol, String]('foo -> userIdentifier)
      ))
    }
  
    "throw exception when data json is invalid" in {
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

  "Edge" can {
    sequential
    
    "be added" in {
      val affogato = Affogato(apiKey)
      affogato.set("foo", userIdentifier)
      affogato.set("barm", bugsIdentifier)

      affogato.set(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier
      ) must beRight
    }
    
    "be added by class" in {
      val affogato = Affogato(apiKey)
      var e: Either[Respond, Edge] = null

      for (
        u <- affogato.set("foo", userIdentifier).right;
        m <- affogato.set("barm", bugsIdentifier).right
      ) {
        e = affogato.set(u, verb="listen", m)
      }

      if(e == null) {
        true === false
      } else {
        e must beRight
      }
    }

    "be added with LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)

      val a: AffogatoResult = affogato.set(LinkedHashMap[String, String](
        "foo" -> userIdentifier,
        "do" -> "listen",
        "barm" -> bugsIdentifier
      ))

      resultEdgeMatcher(a)
    }

    "be added with LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      resultEdgeMatcher(
        affogato.set(LinkedHashMap[Symbol, String](
          'foo -> userIdentifier,
          'do -> "listen",
          'barm -> bugsIdentifier
        ))
      )
    }

    "be found" in {
      val affogato = Affogato(apiKey)
      affogato.get(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier
      ) must beRight
    }

    "be found without inner point option" in {
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

    "be found by LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)
      val getRes = affogato.get(
        LinkedHashMap[String, String](
          "foo" -> userIdentifier,
          "verb" -> "listen",
          "barm" -> bugsIdentifier
        )
      )

      resultListEdgeMatcher(getRes)
    }

    "be found by LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      val getRes = affogato.get(
        LinkedHashMap[Symbol, String](
          'foo -> userIdentifier,
          'verb -> "listen",
          'barm -> bugsIdentifier
        )
      )

      resultListEdgeMatcher(getRes)
    }
    
    "be found with ?" in {
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
    
    "be found with limit, offset options" in {
      val affogato = Affogato(apiKey)

      affogato.set(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier
      ) must beRight

      val e = affogato.get(None, "foo", userIdentifier,
                           "listen", None, "barm", "bugs-identifier1",
                           limit=100, offset=0)
      e must beRight
    }

    "be found with limit, offset by LinkedHashMap" in {
      val affogato = Affogato(apiKey)
      
      resultListEdgeMatcher(affogato.get(LinkedHashMap(
        "foo" -> userIdentifier,
        "verb" -> "listen",
        "barm" -> bugsIdentifier,
        "limit" -> 2.toString(),
        "offset" -> 0.toString()
      ), false))
    }
  }
}
