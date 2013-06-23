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
import scala.concurrent.ExecutionContext.Implicits.global

import com.typesafe.config._
import dispatch._
import net.liftweb.json._
import net.liftweb.json.Serialization.write
import net.liftweb.json.JsonDSL._


import scala.reflect.runtime.universe.{TypeTag, Type, TypeRef, typeOf}

/** Base class of mintpresso response. Point, Edge MUST extend this class.
 *
 * @param code a status code from mintpresso response 200 or 201 or 400 and etc.
 * @param message a status message from mintpresso response
 */
class Respond(var code: BigInt = 0, var message: String = "") {
  /** Set status via functions
   *
   * {{{
   * scala> val r = new Respond()
   * Respond = Respond
   * 
   * scala> r.setStatus((200, "complete")) 
   * }}}
   *
   */
  def setStatus(s: (BigInt, String)) = {
    code = s._1
    message = s._2
  }
}

/** Represent a mintpresso point.
 *
 * @param id mintpresso point id.
 * @param _type mintpresso point's type 
 * @param identifier mintpresso  point's identifier. identifier should be unique
 * @param data mintpresso mintpresso point's additional data. json string
 * @param url mintpresso mintpresso point's url
 * @param updatedAt updated time, unix timestamp
 * @param referencedAt referenced edges, unix timestamp
 * @param createdAt created time, unixtimestamp
 *
 */
case class Point(id: BigInt, _type: String,
                 identifier: String, data: String, url: String, 
                 createdAt: BigInt, updatedAt: BigInt, 
                 referencedAt: BigInt) extends Respond

/** Contain additional information for Point
 *
 * @param points list of [[com.mintpresso.Point]]
 * @param len length of points
 * @param previous previous points when limit & offset given
 * @param current previous points when limit & offset given
 * @param next next points when limit & offset given
 * @param size total size of point found
 *
 */

case class Points(points: List[Point], len: BigInt, previous: String,
                  current: String, next: String, size: BigInt)

/** Represent a mintpresso edge. edge define realation between a points.
 *
 * @param subject a subject point.
 * @param verb describe about relation between subject point
 *             and object point. (eg. person `listen` music)
 * @param _object a object point.
 * @param len length of edge
 * @param url a edge url
 * @param createdAt created time, unix timestamp
 *
 */
case class Edge(subject: Point, verb: String, _object: Point, data: String,
                url: String, createdAt: BigInt) extends Respond

/** Contain additional information about Edge
 *
 * @param edges list of [[com.mintpresso.Edge]]
 * @param len length of edges`
 * @param previous previous edges when limit & offset given
 * @param current current edges when limit & offset given
 * @param next next edges when limit & offset given
 * @param size total size of edges found
 *
 */

case class Edges(edges: List[Edge], len: BigInt,
                 previous: String, current: String,
                 next: String, size: BigInt) extends Respond

/** Result class that can contain Eitner[Respond, Any].
 *
 * {{{
 * scala> val a = AffogatoResult(Right(Point(...)))
 * a: AffogatoResult = AffogatoResult(Right(Point(...)))
 *
 * scala> a.as[Point]
 * Point = Point(...)
 *
 * scala> a.fold(e => println(e), (s: Point) => println(s))
 * Point(...)
 *
 * }}}
 */
case class AffogatoResult(var result: Either[Respond, _]) {
  /** Return result as a instance of type T. if result is not Right(d: T), then it will throw [[com.mintpresso.AffogatoConversionException]].
   *  and it can throw [[java.lang.ClassCastException]] when type T is inappropriate for result.
   *
   * {{{
   * scala> a.as[Point]
   * Point = Point(...)
   *
   * scala> a.as[Boolean]
   * java.lang.ClassCastException: Either[Respond, Point] cannot be cast to java.lang.Boolean
   *
   * }}}
   */
  def as[T] = result.asInstanceOf[Either[Respond, T]] match {
    case Right(d) => d
    case Left(a) => throw new AffogatoConversionException(s"Conversion Failed :`( found: Left($a)")
  }

  /** Handling Error or Success of AffogatoResult. (Respond => R) will call when request fail.
   *
   * {{{
   * scala> a.fold(e => println(e), (s: Point) => println(s))
   * Point(...)
   *
   * scala> val b = AffogatoResult(Left(new Respond(500, "some problem occured")))
   * b: AffogatoResult = AffogatoResult(Left(Respond(...)))
   *
   * scala> b.fold(e => println(e.code, e.message), (s: Point) => println(s))
   * (500, some problem occured)
   * }}}
   */
  def fold[R, T](err: Respond => R, success: T => R): R = {
    try {
      result.asInstanceOf[Either[Respond, T]] match {
        case Right(d) => success(d)
        case Left(e: Respond) => err(e)
      }
    } catch {
      case e: Exception => throw new Exception(s"Exception: $e, Unexpected exception occured. AffogatoResult MUST be Either[Respond, T]")
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

  /** Convert LinkedHashMap[T, String] to Map[String, String] for syntax sugar
   *
   * @param x LinkedHashMap[T, String] to be replaced LinkedHashMap[String, String]. T should be one of [String, Symbol]
   * @return a LinkedHashMap[String, String]
   *
   */
  implicit def tToStringMap[T](m: LinkedHashMap[T, String]): LinkedHashMap[String, String] = m.map { a =>
    a._1 match {
      case x: String => (x -> a._2)
      case x: Symbol => (x.name -> a._2)
      case x => throw new Exception(x.getClass.toString() + " is invalid type for Affogato.set(Map[T,String])")
    }
  }  

  /** Request a http request to given url. it will handling http status and mintpresso response status.
   *  type T should be a type of data returned.
   *  
   */
  private def Request[T](f: ((BigInt, String)) => JValue => Either[Respond, T])(implicit req: com.ning.http.client.RequestBuilder): Either[Respond, T] = {
    Http(req OK as.String).either() match {
      case Right(r: String) => {
        val json = parse(r)
        var st = statusRead(json)
        if(st.isEmpty) {
          Left(new Respond(500, ""))
        } else {
          val status = st.head
          if(status._1 == 201 || status._1 == 200) {
            f(status)(json)
          } else {
            Left(new Respond(status._1, status._2))
          }
        }
        
      }
      case Left(err: StatusCode) => {
        val rawURL = req.build().getRawUrl
        val msg = err.getMessage
        Left(new Respond(err.code, s"$msg in `$rawURL`"))
      }
      case Left(e) => {
        Left(new Respond(500, e.getMessage))
      }
    }
  }
  
  /** Add a point to mintpresso
   *
   * {{{
   * scala> val c = affogato.set("user", "admire9@gmail.com")
   * c: Either[Respond, Point] = Right(Point(...))
   *
   * }}} 
   *
   * @param _type type of point
   * @param identifier identifier of point
   * @param data additional data of point
   * @param updateIfExists updateIfExists flag. FYI, reference mintpresso document.
   * @return a Either[Respond, Point]
   *
   */
  def set(_type: String, identifier: String, data: String="{}", updateIfExists: Boolean=true): Either[Respond, Point] = {
    val parsedData = parse(data)
    if(parsedData == JNothing) throw new AffogatoInvalidJsonException("A point data is invalid. data: String = %s".format(data))

    val point = 
      ("point" -> 
        ("type" -> _type) ~
        ("identifier" -> identifier) ~
        ("data" -> parsedData))

    val additionalData: String = compact(render(point))
    val postPointURI = uri(affogatoConf("mintpresso.url.point").format(accountId))
    implicit var req = url(postPointURI).POST
    req = req.addQueryParameter("api_token", token)
    req = req.addQueryParameter("updateIfExists", updateIfExists.toString)
    req = req.addHeader("Content-Type", "application/json;charset=utf-8")
    req = req << additionalData
    req.setBodyEncoding("utf-8")

    Request[Point] { implicit status => json =>
      Right(addPointRead(json))
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
   * scala> affogato.set(Map[Symbol, String]('user -> "admire93@gmail.com", 'verb -> "like", 'post -> "some-post"))
   * AffogatoResult = AffogatoResult(Edge(...))
   *
   * }}} 
   *
   * @param d LinkedHashMap contain information of mintpresso data.
   * @return AffogatoResult(Point(...)) or AffogatoResult(Edge(...))
   *
   */
  def set[T](d: LinkedHashMap[T, String], updateIfExists: Boolean=true): AffogatoResult = { 
    val stringMap: LinkedHashMap[String, String] = d
    var data: JObject = null

    if((stringMap.keySet & verbSet).isEmpty) {
      var typeIdentifier: (String, String) = null
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

      val res: Either[Respond, Point] = set(
        typeIdentifier._1, typeIdentifier._2,
        compact(render(data)), updateIfExists
      )
      AffogatoResult(res)
    } else {
      var sP: (String, String) = null
      var verb: String = null
      var oP: (String, String) = null

      for( (pair, index) <- stringMap.zipWithIndex ) {
        index match {
          case 0 => sP = pair
          case 1 => verb = pair._2
          case 2 => oP = pair
          case _ => {
            if(data == null) {
              data = pair
            } else {
              data = data ~ pair
            }
          }
        }
      }

      val res: Either[Respond, Edge] = set(-1, sP._1, sP._2, verb,
                                           -1, oP._1, oP._2,
                                           compact(render(data)))
      AffogatoResult(res)
    }
  }

  /** Add a point to mintpresso by [[com.mintpresso.Point]]
   *
   * {{{
   * scala> affogato.set(Point( ... ))
   * Either[Respond, Point] = Right(Point(...))
   *
   * }}} 
   *
   * @param point [[com.mintpresso.Point]] instance 
   * @return Either[Respond, Point]
   *
   */
  def set(point: Point): Either[Respond, Point] = { 
    set(point._type, point.identifier, point.data)
  }


  /** Add a Edge to mintpresso
   *
   * {{{
   * scala> affogato.set("user", "admire9@gmail.com", "listen", "music", "bugs-123", "{}")
   * Either[Respond, Edge] = Right(Edge(...))
   *
   * }}} 
   *
   * @param subjectId id of subject point
   * @param subjectType type of subject point
   * @param subjectIdentifier identifier of subject point
   * @param verb describe about relation between subject point 
   *        and object point. (eg. person `listen` music)
   * @param objectId id of object point
   * @param objectType type of object point
   * @param objectIdentifier identifier of object point
   * @param data weight of edge or other additional data
   * @return Either[Respond, Edge]
   *
   */
  def set(subjectId: Long, subjectType: String,
          subjectIdentifier: String,
          verb: String, objectId: Long, objectType: String,
          objectIdentifier: String,
          data: String): Either[Respond, Edge] = {
    var parsedData = parse("{}")
    if(data != "null" && data.length != 0) {
      parsedData = parse(data)
    }
    var edge: (String, net.liftweb.json.JsonAST.JObject) = null
    if(subjectId != -1 && objectId != -1) {
      edge = 
        ("edge" -> 
          ("subjectId" -> subjectId) ~
          ("subjectType" -> subjectType) ~
          ("objectId" -> objectId) ~
          ("objectType" -> objectType) ~
          ("verb" -> verb) ~
          ("data" -> parsedData))

    } else if(subjectIdentifier.length != 0 && objectIdentifier !=0) {
      edge = 
        ("edge" -> 
          ("subjectIdentifier" -> subjectIdentifier) ~
          ("subjectType" -> subjectType) ~
          ("objectIdentifier" -> objectIdentifier) ~
          ("objectType" -> objectType) ~
          ("verb" -> verb) ~
          ("data" -> parsedData))
    } else {
      throw new AffogatoInvalidParamException("one of [`subjectId`, `objectId`] or [`subjectIdentifier`, `objectIdentifier`] MUST be given")
    }
   
    val addEdgeURI = uri(affogatoConf("mintpresso.url.edge").format(accountId))
    implicit var req = url(addEdgeURI).POST
    req << compact(render(edge))
    req.addQueryParameter("api_token", token)     
    req.addHeader("Content-Type", "application/json;charset=utf-8")
    req.setBodyEncoding("utf-8")

    Request[Edge] { implicit status => json =>
      Right(Edge(
        Point(-1, subjectType, subjectIdentifier, "", "", 0, 0, 0),
        verb,
        Point(-1, objectType, objectIdentifier, "", "", 0, 0, 0),
        "",
        "",
        0
      ))
    }
  }
  
  /** Add a Edge to mintpresso by [[com.mintpresso.Point]]
   *
   * {{{
   * scala> affogato.set(Point(...), "like", Point(...), "{}")
   * Either[Respond, Edge] = Right(Edge(...))
   *
   * }}}
   *
   * @param subject [[com.mintpresso.Point]] that contains data about subject of realation
   * @param verb describe about relation between subject point and object point
   * @param _object [[com.mintpresso.Point]] that contains data about object of realation
   * @param data additional data of point
   * @return Either[Respond, Edge]]
   *
   */
  def set(subject: Point, verb: String, _object: Point,
          data: String): Either[Respond, Edge] = {
    set(-1, subject._type, subject.identifier, verb,
        -1, _object._type, _object.identifier, data)
  }

  /** Get a point by id
   *
   * {{{
   * scala> affogato.get(1234)
   * Either[Respond, Point] = Right(Point(...))
   *
   * }}} 
   *
   * @param id id of point
   * @return Either[Respond, Point]
   *
   */
  def get(id: BigInt): Either[Respond, Point] = {
    val getPointURI = uri(affogatoConf("mintpresso.url.point.find.id").format(accountId, id))
    implicit var req = url(getPointURI)
    req.addQueryParameter("api_token", token)

    Request[Point] { implicit status => json =>
      Right(addPointRead(json))
    }
  }

  /** Get a point by type or identifier
   *
   * {{{
   * scala> val foo = affogato.get("foo", "bar", -1, -1)
   * foo: Either[Respond, Points] = Right(Points(...))
   *
   * scala> for(p <- foo.right) println(p.points.head.identifier)
   * foo
   *
   * scala> affogato.get("foo", "bar", 10, 5)
   * Either[Respond, Points] = Right(Points(...))
   *
   * scala> for(p <- foo.right) println(p)
   * Points(List[Point](Point(...), 5, "http://mintpresso.com...", "...", "...", 10)
   *
   * }}} 
   *
   * @param _type type of point
   * @param identifier identifier of point
   * @return Either[Respond, Points]
   *
   */
  def get(_type: String, identifier: String,
          limit: Long, offset: Long): Either[Respond, Points] = {
    val getPointURI = uri(affogatoConf("mintpresso.url.point").format(accountId))
    implicit var req = url(getPointURI)
    req.addQueryParameter("api_token", token)
    if(_type != "?") {
      req.addQueryParameter("type", _type)
    }
    if(identifier != "?") {
      req.addQueryParameter("identifier", identifier)
    }
    if(limit != -1) {
      req.addQueryParameter("limit", limit.toString)
    }
    if(offset != -1) {
      req.addQueryParameter("offset", offset.toString)
    }

    req.addHeader("Accepts", "application/json;charset=utf-8")    

    Request[Points] { implicit status => json =>
      if(_type != "?" && identifier != "?") {
        Right(pointRead(json))
      } else {
        Right(pointsRead(json))
      }
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
   * @param limit limit of Edge result
   * @param offset offset of Edge result
   * @param getInnerPoints getInnerPoints flag. if flag is true, edge data filled with real Point data.
   * @return Either[Respond, Edges]
   *
   */
  def get(subjectId: Option[Long] = None, subjectType: String, 
          subjectIdentifier: String, verb: String,
          objectId: Option[Long] = None, objectType: String,
          objectIdentifier: String, limit: Long = 100, offset: Long = 0,
          oldest: String = "", newest: String = "",
          getInnerPoints: Boolean = true): Either[Respond, Edges] = {
    val getEdgeURI = uri(affogatoConf("mintpresso.url.edge").format(accountId))
    implicit val req = url(getEdgeURI)
    req.addQueryParameter("api_token", token)
    if(subjectIdentifier != "?") req.addQueryParameter("subjectIdentifier",
                                                       subjectIdentifier)

    if(objectIdentifier != "?") req.addQueryParameter("objectIdentifier",
                                                      objectIdentifier)

    req.addQueryParameter("subjectType", subjectType)
    req.addQueryParameter("objectType", objectType)
    req.addQueryParameter("verb", verb)
    req.addQueryParameter("getInnerPoints", getInnerPoints.toString)
    req.addQueryParameter("limit", limit.toString)
    req.addQueryParameter("offset", offset.toString)

    if(newest.length != 0) {
      req.addQueryParameter("newest", newest)
    }
    if(oldest.length != 0) {
      req.addQueryParameter("oldest", oldest)
    }

    subjectId.map { sId =>
      req.addQueryParameter("subjectId", sId.toString) 
    }

    objectId.map { oId =>
      req.addQueryParameter("objectId", oId.toString) 
    }
    Request[Edges] { implicit status => json =>
      Right(edgeRead(json))
    }
  }

  /** Get a point or Edge by LinkedHashMap[String, String] or LinkedHashMap[Symbol, String]
   *
   * @param d LinkedHashMap[T, String] data about Point or Edge. 
   * @param getInnerPoints getInnerPoints flag.
   *
   * {{{
   * scala> affogato.get(LinkedHashMap[String, String](
   *          "user" -> "admire93"
   *        ))
   * AffogatoResult[Either[Respond, Point]] = AffogatoResult[Either[Respond, Point]](Right[Point(...))
   *
   * scala> affogato.get(LinkedHashMap[Symbol, String](
   *          'user -> "admire93",
   *          'verb -> "listen",
   *          'music -> "bugs-1"
   *        ))
   * AffogatoResult[Either[Respond, Edges]] = AffogatoResult[Either[Respond, Edges]](Right[Point(...))
   *
   * scala> affogato.get(LinkedHashMap[Symbol, String](
   *          'user -> "admire93",
   *          'verb -> "listen",
   *          'music -> "bugs-1",
   *          'limit -> "100",
   *          'offset -> "0"
   *        ), getInnerPoints=false)
   * AffogatoResult[Either[Respond, Edges]] = AffogatoResult[Either[Respond, Edges]](Right[Point(...))
   *
   * }}}
   *
   */
  def get[T](d: LinkedHashMap[T, String], getInnerPoints: Boolean=true): AffogatoResult = { 
    val stringMap: LinkedHashMap[String, String] = d

    if((stringMap.keySet & verbSet).isEmpty) {
      var typeIdentifier: (String, String) = null
      var limit: Long = -1
      var offset: Long = -1
      
      for((pair, index) <- stringMap.zipWithIndex) {
        index match {
          case 0 =>  typeIdentifier = pair
          case _ => pair._1 match {
            case "limit" => limit = pair._2.toLong
            case "offset" => offset = pair._2.toLong 
          }
        }
      }

      AffogatoResult(get(typeIdentifier._1, typeIdentifier._2, limit, offset))
    } else {
      var sP: (String, String) = null
      var verb: String = null
      var oP: (String, String) = null
      var sId: (String, String) = null
      var oId: (String, String) = null
      var limit: Long = 10
      var offset: Long = 0
      var newest = ""
      var oldest = ""
      var res: Either[Respond, Edges] = null

      for((pair, index) <- stringMap.zipWithIndex) {
        index match {
          case 0 => {
            if(pair._1 == "subjectId") {
              sId = pair
            } else {
              sP = pair
            }
          }
          case 1 => verb = pair._2
          case 2 => {
            if(pair._1 == "objectId") {
              oId = pair
            } else {
              oP = pair
            }
          }
          case _ => pair._1 match {
            case "limit" => limit = pair._2.toLong
            case "offset" => offset = pair._2.toLong
            case "newest" => newest = pair._2
            case "oldest" => oldest = pair._2
          }
        }
      }

      if(sP == null) {
        res = get(Some(sId._2.toLong), "", "", verb, 
                  Some(oId._2.toLong), "", "", limit, offset, 
                  oldest, newest, getInnerPoints)
      } else {
        res = get(None, sP._1, sP._2, verb, None, oP._1, oP._2,
                  limit, offset, oldest, newest, getInnerPoints)
      }

      AffogatoResult(res)
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
  
  private def pointsRead(json: JValue)(implicit status: (BigInt, String)): Points = {
    var previous = ""
    var current = ""
    var next = ""
    for(n <- (json \ "next" \\ classOf[JString])) {
      next = n
    }
    for(n <- (json \ "previous" \\ classOf[JString])) {
      previous = n
    }
    for(n <- (json \ "current" \\ classOf[JString])) {
      current = n
    }
    val points = for {
        JString(url) <- json \ "url"
        JInt(length) <- json \ "length"
        JInt(size) <- json \ "size"
        JObject(point) <- json \ "points"
    } yield Points((for {
        JField("id", JInt(i)) <- point
        JField("type", JString(t)) <- point
        JField("identifier", JString(iden)) <- point
        JField("data", JObject(data)) <- point
        JField("createdAt", JInt(ca)) <- point
        JField("updatedAt", JInt(ua)) <- point
        JField("referencedAt", JInt(ra)) <- point
    } yield Point(i, t, iden, compact(render(data)), url, ca, ua, ra)), length, previous, current, next, size)

    points.head
  }
  private def addPointRead(json: JValue)(implicit status: (BigInt, String)): Point = {
    var it = for {
      JField("point", JObject(point)) <- json
      JField("id", JInt(i)) <- point
      JField("type", JString(t)) <- point
      JField("identifier", JString(iden)) <- point
      JField("data", JObject(data)) <- point
      JField("url", JString(url)) <- point
      JField("createdAt", JInt(ca)) <- point
      JField("updatedAt", JInt(ua)) <- point
      JField("referencedAt", JInt(ra)) <- point
    } yield Point(i, t, iden, compact(render(data)), url, ca, ua, ra)

    it.head
  }


  private def pointRead(json: JValue)(implicit status: (BigInt, String)): Points = {
    var it = for {
      JString(url) <- json \ "url"
      JInt(length) <- json \ "length"
      JObject(point) <- json \\ "point"
    } yield Points((for {
      JField("id", JInt(i)) <- point
      JField("type", JString(t)) <- point
      JField("identifier", JString(iden)) <- point
      JField("data", JObject(data)) <- point
      JField("createdAt", JInt(ca)) <- point
      JField("updatedAt", JInt(ua)) <- point
      JField("referencedAt", JInt(ra)) <- point
    } yield Point(i, t, iden, compact(render(data)), url, ca, ua, ra)), length, "", "", "", length)

    it.head
  }

  private def statusRead(json: JValue): List[(BigInt, String)] = (for {
    JObject(status) <- json \ "status"
    JField("code", JInt(c)) <- status 
    JField("message", JString(s)) <- status
  } yield (c, s)).toList 

  private def edgeRead(json: JValue)(implicit status: (BigInt, String)): Edges = {
    val edges = json \ "edges"
    val jPrevious = (json \ "previous" \\ classOf[JString])
    val jLen = (json \ "length" \\ classOf[JInt])
    val jNext = (json \ "next" \\ classOf[JString])
    val jCurrent = (json \ "current" \\ classOf[JString])
    val jSize = (json \ "size" \\ classOf[JInt])
    var len: BigInt = 0
    var size: BigInt = 0
    var current = ""
    var previous = ""
    var next = ""

    if(!jLen.isEmpty) len = jLen.head.asInstanceOf[BigInt]
    if(!jPrevious.isEmpty) previous = jPrevious.head.asInstanceOf[String]
    if(!jCurrent.isEmpty) current = jCurrent.head.asInstanceOf[String]
    if(!jNext.isEmpty) next = jNext.head.asInstanceOf[String]
    if(!jSize.isEmpty) size = jSize.head.asInstanceOf[BigInt]

    implicit val formats = Serialization.formats(NoTypeHints)
    var res: List[Edge] = List[Edge]()

    edges.values.asInstanceOf[List[Map[String, Any]]].map { edge =>
      var verb = edge("verb").asInstanceOf[String]
      var url = edge.get("url").getOrElse("").asInstanceOf[String]
      var createdAt = edge("createdAt").asInstanceOf[BigInt]
      edge.get("subject").map { s =>
        val o = edge("object")
        val subject = s.asInstanceOf[Map[String, Any]] 
        val _object = o.asInstanceOf[Map[String, Any]]
        val e: Edge = Edge(
          Point(
            subject("id").asInstanceOf[BigInt],
            subject("type").asInstanceOf[String],
            subject("identifier").asInstanceOf[String],
            write(subject("data").asInstanceOf[Map[String, String]]),
            subject.get("url").getOrElse("").asInstanceOf[String],
            subject("createdAt").asInstanceOf[BigInt],
            subject("updatedAt").asInstanceOf[BigInt],
            subject("referencedAt").asInstanceOf[BigInt]
          ),
          verb,
          Point(
            _object("id").asInstanceOf[BigInt],
            _object("type").asInstanceOf[String],
            _object("identifier").asInstanceOf[String],
            write(_object("data").asInstanceOf[Map[String, String]]),
            _object.get("url").getOrElse("").asInstanceOf[String],
            _object("createdAt").asInstanceOf[BigInt],
            _object("updatedAt").asInstanceOf[BigInt],
            _object("referencedAt").asInstanceOf[BigInt]
          ),
          "",
          url,
          createdAt
        )
        res = e :: res
      }.getOrElse{
        def pointURI(i: Long): String = uri(affogatoConf("mintpresso.url.point.find.id").format(accountId, i))
        val subjectId = edge("subjectId").asInstanceOf[BigInt]
        val objectId = edge("objectId").asInstanceOf[BigInt]
        res = Edge(
          Point(
            subjectId,
            edge("subjectType").asInstanceOf[String],
            "",
            "",
            pointURI(subjectId.toLong),
            0,
            0,
            0
          ),
          verb, 
          Point(
            objectId,
            edge("objectType").asInstanceOf[String],
            "",
            "",
            pointURI(objectId.toLong),
            0,
            0,
            0
          ),
          "",
          url,
          createdAt
        ) :: res
      }
    }
    val es = Edges(res, len, previous, current, next, size)
    es.setStatus(status)

    return es
  }  
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

/**  Parameter Exception class. throw when Parameter is invalid
 *
 */
class AffogatoInvalidParamException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
    def this() = this("", null)
     
    def this(message: String) = this(message, null)
     
    def this(nestedException : Throwable) = this("", nestedException)
}
