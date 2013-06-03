package com.mintpresso
/** Provides Scala API for mintpresso.com.
  * Affogato provides more simple and easy way to use mintpresso
  * 
  * ==Overview==
  * 
  * A Affogato class should be initialized with api key can get from mintpresso.com.
  * 
  * {{{
  * scala> import com.mintpresso._
  * scala> val affogato: Affogato = new Affogato("some-api-key-goes-here") 
  * affogato: com.mintpresso.Affogato = com.mintpresso.Affogato
  * 
  * }}} 
  */

import scala.language.implicitConversions
import scala.collection.mutable.LinkedHashMap
import java.util.Date

import com.typesafe.config._
import dispatch._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

import scala.reflect.runtime.universe.{TypeTag, Type, TypeRef, typeOf}

class Respond {
  var code: BigInt = null
  var message: String = null

  def setStatus(c: BigInt, m: String) = {
    code = c
    message = m
  }

  def setStatus(d: (BigInt, String)) = {
    code = d._1
    message = d._2
  }
}

/** Represent a mintpresso point.
 *
 * @param id mintpresso point id.
 * @param _type mintpresso point's type 
 * @param identifier mintpresso  point's identifier. identifier should be unique
 * @param data mintpresso mintpresso point's additional data. json string
 * @param _url mintpresso mintpresso point's url
 * @param updatedAt updated time, unix timestamp
 * @param referencedAt referenced edges, unix timestamp
 * @param createdAt created time, unixtimestamp
 *
 */
case class Point(id: BigInt, _type: String, identifier: String,
                 data: String, _url: String, createdAt: BigInt,
                 updatedAt: BigInt, referencedAt: BigInt) extends Respond

/** Represent a mintpresso edge. edge define realation between a points.
 *
 * @param subject a subject point.
 * @param verb describe about relation between subject point
 *             and object point. (eg. person `listen` music)
 * @param _object a object point.
 * @param url a edge url
 * @param url created time, unix timestamp
 *
 */
case class Edge(subject: Point, verb: String, _object: Point,
                url: String, createdAt: BigInt) extends Respond

case class AffogatoResult(var result: Any) {
  def as[T] = result.asInstanceOf[Either[Respond, T]] match {
    case Right(d) => d
    case Left(a) => throw new AffogatoConversionException(s"Conversion Failed :`( found: $a require: $result")
  }

  def fold[R, T](err: Respond => R, success: T => R): R = {
    try {
      result.asInstanceOf[Either[Respond, T]] match {
        case Right(d) => success(d)
        case Left(e) => err(e)
      }
    } catch {
      case e: Exception => throw new Exception("Unexpected exception occured. AffogatoResult MUST be Either[Respond, T]")
    } 
  }
}

/** Affogato is a Mintpresso Scala API Pack.
 */ 
object Affogato {
  val separator = "::" 

  def apply(token: String, accountId: Long): Affogato = new Affogato(token, accountId)

  /** Overloaded constructor
   *
   * @constructor Create a new affogato
   * @param key a key combined api token and accountId (eg. asdf02309::1)
   * @return A instance of Affogato
   *
   * {{{
   * scala> import com.mintpresso._
   * scala> val affogato: Affogato = Affogato("cc55223dfasfe29fs1::1") 
   * affogato: com.mintpresso.Affogato = com.mintpresso.Affogato
   *
   * }}}
   *
   */
  def apply(key: String): Affogato = {
    val splitedKey: Array[String] = key.split(separator)
    new Affogato(splitedKey(0), splitedKey(1).toLong)
  }
}

/** Affogato class
 *
 * {{{
 * scala> import com.mintpresso._
 * scala> val affogato: Affogato = Affogato("cc55223dfasfe29fs", 1) 
 * affogato: com.mintpresso.Affogato = com.mintpresso.Affogato
 *
 * }}}
 *
 * @constructor Create a new affogato with api key
 * @param token mintpresso api key
 * @param accountId mintpresso account id
 * @return A instance of Affogato
 */
class Affogato(val token: String, val accountId: Long) {
  val conf = ConfigFactory.load("affogato")
  val affogatoConf: Map[String, String] = Map[String, String](
    "mintpresso.host" -> conf.getString("mintpresso.host"),
    "mintpresso.port" -> conf.getString("mintpresso.port"),
    "mintpresso.version" -> conf.getString("mintpresso.version"),
    "mintpresso.protocol" -> conf.getString("mintpresso.protocol"),
    "mintpresso.url.point" -> "/account/%d/point",
    "mintpresso.url.edge" -> "/account/%d/edge",
    "mintpresso.url.point.find.id" -> "/account/%d/point/%d"
  )
  val verbSet: Set[String] = Set("verb", "do", "did", "does")

  /** Convert Map[T, String] to Map[String, String] for syntax sugar
   *
   * @param x Map[T, String] to be replaced Map[String, String]. T should be String or Symbol
   * @return a Map[String, String]
   *
   */
  implicit def tToStringMap[T](m: LinkedHashMap[T, String]): LinkedHashMap[String, String] = m.map { a =>
    a._1 match {
      case x: String => (x -> a._2)
      case x: Symbol => (x.name -> a._2)
      case x => throw new Exception(x.getClass.toString() + " is invalid type for Affogato.set(Map[T,String])")
    }
  }

  private def fail(code: BigInt, url: String) = {
    val resp = new Respond()
    resp.setStatus(code, s"HTTP request failed in url: $url.")
    Left(resp)
  }
  
  /** Add a point to mintpresso
   *
   * {{{
   * scala> affogato.set("user", "admire9@gmail.com")
   * Either[Respond, Point] = Right(Point(...))
   *
   * }}} 
   *
   * @param _type type of point
   * @param identifier identifier of point
   * @param data additional data of point
   * @return a AffogatoResult if request success, it will return ResultSet.result: Point
   *
   */
  def set(_type: String, identifier: String, data: String = "{}"): Either[Respond, Point] = {
    val parsedData = parse(data)
    if(parsedData == JNothing) throw new AffogatoInvalidJsonException("A point data is invalid. data: String = %s".format(data))

    val point = 
      ("point" -> 
        ("type" -> _type) ~
        ("identifier" -> identifier) ~
        ("data" -> parsedData))

    val additionalData: String = compact(render(point))
    val postPointURI = uri(affogatoConf("mintpresso.url.point").format(accountId))
    var req = url(postPointURI).POST
    req = req.addQueryParameter("api_token", token)
    req = req.addHeader("Content-Type", "application/json;charset=utf-8")
    req = req << additionalData
    req.setBodyEncoding("utf-8")
    
    Http(req OK as.String).option().map { res =>
      val json = parse(res)
      val status = statusRead(json).head
      if(status._1 == 201 || status._1 == 200) {
        val p = pointRead(json).head
        p.setStatus(status)
        Right(p)
      } else {
        val resp = new Respond()
        resp.setStatus(status)
        Left(resp)
      } 
    }.getOrElse {
      fail(500, postPointURI)
    }
  }
  
  /** Add a point or Edge
   *
   * {{{
   * scala> import scala.collection.mutable.LinkedHashMap
   * scala.collection.mutable.LinkedHashMap
   *
   * scala> affogato.set(LinkedHashMap[String, String]("user" -> "admire9@gmail.com", "name" -> "kanghyojun"))
   * AffogatoResult = AffogatoResult(Point(...))
   *
   * scala> affogato.set(Map[Symbol, String]('user -> "admire93@gmail.com", 'name -> "kanghyojun"))
   * AffogatoResult = AffogatoResult(Point(...))
   *
   * }}} 
   *
   * @param d LinkedHashMap 
   * @return AffogatoResult(Point(...))
   *
   */
  def set[T](d: LinkedHashMap[T, String]): AffogatoResult = { 
    val stringMap: LinkedHashMap[String, String] = d

    if((stringMap.keySet & verbSet).isEmpty) {
      var typeIdentifier: (String, String) = null
      var data: JObject = null
      for( (pair, index) <- stringMap.zipWithIndex ) {
        index match {
          case 0 =>  typeIdentifier = pair
          case _ => {
            if(data == null) {
              data = pair
            } else {
              data = data ~ pair
            }
          }
        }
      }
      AffogatoResult(set(typeIdentifier._1, typeIdentifier._2, compact(render(data))))
    } else {
      var sP: (String, String) = null
      var verb: String = null
      var oP: (String, String) = null

      for( (pair, index) <- stringMap.zipWithIndex ) {
        index match {
          case 0 => sP = pair
          case 1 => verb = pair._2
          case 2 => oP = pair
          case _ => throw new Exception("Unused data contained in data -"+pair)
        }
      }

      AffogatoResult(set(sP._1, sP._2, verb, oP._1, oP._2))
    }
  }

  /** Add a point to mintpresso
   *
   * {{{
   * scala> affogato.set(Point( ... ))
   * Option[Point] = Some(Point(...))
   *
   * }}} 
   *
   * @param point [[com.mintpresso.Point]] instance 
   * @return 
   *
   */
  def set(point: Point): Either[Respond, Point] = { 
    set(point._type, point.identifier, point.data)
  }


  /** Add a Edge to mintpresso
   *
   * {{{
   * scala> affogato.set("user", "admire9@gmail.com", "listen", "music", "bugs-123")
   * Option[Edge] = Some(Edge(...))
   *
   * }}} 
   *
   * @param subjectType type of subject point
   * @param subjectIdentifier identifier of subject point
   * @param verb describe about relation between subject point 
   *        and object point. (eg. person `listen` music)
   * @param objectType type of object point
   * @param objectIdentifier identifier of object point
   * @return a value whether request is successfully ended or not
   *
   */
  def set(subjectType: String, subjectIdentifier: String, verb: String,
          objectType: String, objectIdentifier: String): Either[Respond, Boolean] = {
    val edge = 
      ("edge" -> 
        ("subjectId" -> subjectIdentifier) ~
        ("subjectType" -> subjectType) ~
        ("objectId" -> objectIdentifier) ~
        ("objectType" -> objectType) ~
        ("verb" -> verb))

    val addEdgeURI = uri(affogatoConf("mintpresso.url.edge").format(accountId))
    val req = url(addEdgeURI).POST
    req << compact(render(edge))
    req.addQueryParameter("api_token", token)     
    req.addHeader("Content-Type", "application/json;charset=utf-8")    
    req.setBodyEncoding("utf-8")

    Http(req OK as.String).option().map { res =>
      val d: JValue = parse(res)

      val r = for {
        JObject(status) <- d \\ "status"
        JField("code", JInt(code)) <- status
      } yield code

      Right(true)
    }.getOrElse {
      Left(new Respond())
    }
  }
  
  /** Add a Edge to mintpresso
   *
   * {{{
   * scala> affogato.set(Point(...), "like", Point(...))
   * ResultSet = ResultSet(Edge(...))
   *
   * }}} 
   *
   * @param subject Point
   * @param verb describe about relation between subject point 
   * @param _object Point
   * @return ResultSet(Edge(...))
   *
   */
  def set(subject: Point, verb: String, _object: Point): Either[Respond, Boolean] = {
    val edge = 
      ("edge" -> 
        ("subjectId" -> subject.identifier) ~
        ("subjectType" -> subject._type) ~
        ("objectId" -> _object.identifier) ~
        ("objectType" -> _object._type) ~
        ("verb" -> verb))

    val addEdgeURI = uri(affogatoConf("mintpresso.url.edge").format(accountId))
    val req = url(addEdgeURI).POST
    req << compact(render(edge))
    req.addQueryParameter("api_token", token)     
    req.addHeader("Content-Type", "application/json;charset=utf-8")    
    req.setBodyEncoding("utf-8")

    Http(req OK as.String).option().map { res =>
      val d: JValue = parse(res)

      val r = for {
        JObject(status) <- d \ "status"
        JField("code", JInt(code)) <- status
        JField("message", JString(message)) <- status
      } yield (code, message)
      // Fix it

      Right(true)
    }.getOrElse {
      fail(500, addEdgeURI)
    }
  }
  /** Get a point by id
   *
   * @param id id of point
   * @return a ResultSet if point is exist, ResultSet(Option[Point]) will return.
   *
   */
  def get(id: Long): Option[Point] = {
    val getPointURI = uri(affogatoConf("mintpresso.url.point.find.id").format(accountId, id))
    var req = url(getPointURI)
    req.addQueryParameter("api_token", token)

    Http(req OK as.String).option().map { res =>
      val json = parse(res)
      val point = for {
        JObject(point) <- json \ "point"
        JField("id", JInt(i)) <- point 
        JField("type", JString(t)) <- point 
        JField("identifier", JString(iden)) <- point 
        JField("data", JObject(data)) <- point
        JField("_url", JString(u)) <- point
        JField("createdAt", JInt(ca)) <- point 
        JField("updatedAt", JInt(ua)) <- point 
        JField("referencedAt", JInt(ra)) <- point 
      } yield Point(i.toLong, t, iden, compact(render(data)), u, ca, ua, ra)
      Some(point.head)
    }.getOrElse {
      None
    }
  }

  /** Get a point
   *
   * @param _type type of point
   * @param identifier identifier of point
   * @return a Option[point] if point is exist, Some(Point) will return.
   *
   */
  def get(_type: String, identifier: String): Either[Respond, Point] = {
    val getPointURI = uri(affogatoConf("mintpresso.url.point").format(accountId))
    var req = url(getPointURI)
    req.addQueryParameter("api_token", token)
    req.addQueryParameter("type", _type)
    req.addQueryParameter("identifier", identifier)

    Http(req OK as.String).option().map { res =>
      val json = parse(res)
      val point = for {
        JObject(point) <- json \ "point"
        JField("id", JInt(i)) <- point 
        JField("type", JString(t)) <- point 
        JField("identifier", JString(iden)) <- point 
        JField("data", JObject(data)) <- point
        JField("_url", JString(u)) <- point
        JField("createdAt", JInt(ca)) <- point 
        JField("updatedAt", JInt(ua)) <- point 
        JField("referencedAt", JInt(ra)) <- point 
      } yield Point(i.toLong, t, iden, compact(render(data)), u, ca, ua, ra)
      Right(point.head)
    }.getOrElse {
      fail(500, getPointURI)
    }
  }

  /** Get a edge from mintpresso
   *
   * @param subjectId id of subject point
   * @param subjectType type of subject point
   * @param subjectIdentifier identifier of subject point
   * @param verb describe about relation between subject point 
   *        and object point. (eg. person `listen` music)
   * @param objectId id of object point
   * @param objectType type of object point
   * @param objectIdentifier identifier of object point
   * @return a value whether request is successfully ended or not
   *
   */
  def get(subjectId: Option[Long] = None, subjectType: String, 
          subjectIdentifier: String, verb: String,
          objectId: Option[Long] = None, objectType: String,
          objectIdentifier: String): Option[List[Edge]] = {
    val getEdgeURI = uri(affogatoConf("mintpresso.url.edge").format(accountId))
    val req = url(getEdgeURI)
    req.addQueryParameter("api_token", token)
    if(subjectIdentifier != "?") req.addQueryParameter("subjectIdentifier",
                                                       subjectIdentifier)

    if(objectIdentifier != "?") req.addQueryParameter("objectIdentifier",
                                                      objectIdentifier)

    req.addQueryParameter("subjectType", subjectType)
    req.addQueryParameter("objectType", objectType)
    req.addQueryParameter("verb", verb)

    subjectId.map { sId =>
      req.addQueryParameter("subjectId", sId.toString) 
    }

    objectId.map { oId =>
      req.addQueryParameter("objectId", oId.toString) 
    }

    Http(req OK as.String).option().map { res =>
      def pointURI(i: Long): String = uri(affogatoConf("mintpresso.url.point.find.id").format(accountId, i))
      val json = parse(res)
      val r: List[Edge] = for {
        JObject(edges) <- json \\ "edges"
        JField("subjectId", JInt(subjectId)) <- edges
        JField("subjectType", JString(subjectType)) <- edges
        JField("verb", JString(verb)) <- edges
        JField("objectId", JInt(objectId)) <- edges
        JField("objectType", JString(objectType)) <- edges
        JField("_url", JString(url)) <- edges
        JField("createdAt", JInt(createdAt)) <- edges
      } yield Edge(
                Point(
                  subjectId.toLong,
                  subjectType,
                  subjectIdentifier,
                  "",
                  pointURI(subjectId.toLong),
                  0,
                  0,
                  0
                ),
                verb, 
                Point(
                  objectId.toLong,
                  objectType,
                  objectIdentifier,
                  "",
                  pointURI(objectId.toLong),
                  0,
                  0,
                  0
                ),
                url,
                createdAt
              )
      Some(r)
    }.getOrElse {
      None
    }
  }

  /** Generate a uri for mintpresso api
   *
   * @param path a path under version prefix
   * @return a uri for mintpresso api
   *
   * {{{
   * scala> affogato.uri("/post/account/1/point")
   * String = http://mintpresso.com:90001/v1/post/account/1/point
   * }}} 
   *
   */ 
  def uri(path: String) = {

    "%1$s://%2$s:%3$s/%4$s%5$s".format(
      affogatoConf("mintpresso.protocol"),
      affogatoConf("mintpresso.host"),
      affogatoConf("mintpresso.port"),
      affogatoConf("mintpresso.version"),
      path
    )
  }

  override def toString(): String = "Affogato(%1$s, %2$s)".format(token , accountId)
  
  private def pointRead(json: JValue): List[Point] = {
    var now = new Date()
    var time = now.getTime() 
    val res = for {
      JField("point", point) <- json
      JField("id", JInt(i)) <- point 
      JField("type", JString(t)) <- point 
      JField("identifier", JString(iden)) <- point 
      JField("data", JObject(data)) <- point
      JField("_url", JString(u)) <- point
    } yield new Point(i.toLong, t, iden, compact(render(data)), u, time, time, time)

    res.toList
  }

  private def statusRead(json: JValue): Iterable[(BigInt, String)] = for {
    JObject(status) <- json \ "status"
    JField("code", JInt(c)) <- status 
    JField("message", JString(s)) <- status
  } yield (c, s)

}


/** Invalid Json Exception Class
 *
 */
class AffogatoInvalidJsonException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
    def this() = this("", null)
     
    def this(message: String) = this(message, null)
     
    def this(nestedException : Throwable) = this("", nestedException)
}

/**  Conversion Exception class. throw when ResultSet.result cannot be convert with given Type
 *
 */
class AffogatoConversionException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
    def this() = this("", null)
     
    def this(message: String) = this(message, null)
     
    def this(nestedException : Throwable) = this("", nestedException)
}
