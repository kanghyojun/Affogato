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
  val eitherPointsMatcher = (r: Either[Respond, Points]) => {
    r must beRight.like {
      case p: Points => {
        p.points.length must be_>(0)
      }
    }
  }
  val eitherEdgesMatcher = (r: Either[Respond, Edges]) => {
    r must beRight.like {
      case e: Edges => e.edges.length must be_>(0)
    }
  }
  val resultPointMatcher = (r: AffogatoResult) => {
    r.result.asInstanceOf[Either[Respond, Point]] must beRight
  }
  val resultPointsMatcher = (r: AffogatoResult) => {
    eitherPointsMatcher(r.result.asInstanceOf[Either[Respond, Points]])
  }
  val resultEdgeMatcher = (r: AffogatoResult) => {
    r.result.asInstanceOf[Either[Respond, Edge]] must beRight
  }
  val resultEdgesMatcher = (r: AffogatoResult) => {
    eitherEdgesMatcher(r.result.asInstanceOf[Either[Respond, Edges]])
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
      val res = affogato.get("foo", userIdentifier, -1, -1)
      eitherPointsMatcher(res)
    }

    "be found by id" in {
      val affogato = Affogato(apiKey)
      affogato.get("foo", userIdentifier, -1, -1) match {
        case Right(p) => {
          eitherPointMatcher(affogato.get(p.points.head.id))
        }
        case Left(_) => false === true
      }
    }

    "be found by LinkedHashMap[String, String]" in {
      val affogato = Affogato(apiKey)
      resultPointsMatcher(affogato.get(
        LinkedHashMap[String, String]("foo" -> userIdentifier)
      ))
    }

    "be found by LinkedHashMap[Symbol, String]" in {
      val affogato = Affogato(apiKey)
      resultPointsMatcher(affogato.get(
        LinkedHashMap[Symbol, String]('foo -> userIdentifier)
      ))
    }

    "be found with `?`" in {
      val affogato = Affogato(apiKey)
      resultPointsMatcher(affogato.get(
        LinkedHashMap[Symbol, String]('foo -> "?")
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

      affogato.set(-1, "foo", userIdentifier, "listen",
                   -1, "barm", bugsIdentifier, "{}") must beRight
    }
    
    "be added by class" in {
      val affogato = Affogato(apiKey)
      var e: Either[Respond, Edge] = null

      for (
        u <- affogato.set("foo", userIdentifier).right;
        m <- affogato.set("barm", bugsIdentifier).right
      ) {
        e = affogato.set(u, verb="listen", m, "")
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
      eitherEdgesMatcher(affogato.get(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier
      ))
    }

    "be found without inner point option" in {
      val affogato = Affogato(apiKey)
      eitherEdgesMatcher(affogato.get(
        subjectType="foo",
        subjectIdentifier=userIdentifier,
        verb="listen",
        objectType="barm",
        objectIdentifier=bugsIdentifier,
        getInnerPoints=false
      ))
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

      resultEdgesMatcher(getRes)
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

      resultEdgesMatcher(getRes)
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
      eitherEdgesMatcher(getRes)
    }
    
    "be found with limit, offset options" in {
      val affogato = Affogato(apiKey)

      val e = affogato.get(None, "foo", userIdentifier,
                           "listen", None, "barm", "bugs-identifier1",
                           limit=100, offset=0)
      eitherEdgesMatcher(e)
    }

    "be found with limit, offset by LinkedHashMap" in {
      val affogato = Affogato(apiKey)
      
      resultEdgesMatcher(affogato.get(LinkedHashMap(
        "foo" -> userIdentifier,
        "verb" -> "listen",
        "barm" -> bugsIdentifier,
        "limit" -> 2.toString(),
        "offset" -> 0.toString()
      ), false))
    }
    
    "be found with newest created" in {
      val affogato = Affogato(apiKey)
      val e = affogato.get(None, "foo", userIdentifier,
                           "listen", None, "barm", "bugs-identifier1",
                           limit=10, offset=0, newest="created")

      e must beRight.like {
        case e: Edges => {
          e.edges.length must be_>(0)
          e.edges.head.createdAt must be_>(e.edges(2).createdAt)
        }
      }
    }
    "be found with oldest created" in {
      val affogato = Affogato(apiKey)
      val e = affogato.get(None, "foo", userIdentifier,
                           "listen", None, "barm", "bugs-identifier1",
                           limit=10, offset=0, oldest="created")

      e must beRight.like {
        case e: Edges => {
          e.edges.length must be_>(0)
          e.edges.head.createdAt must be_<(e.edges(2).createdAt)
        }
      }
    }
  }
}
