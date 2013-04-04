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

import dispatch._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

/** Represent a mintpresso point.
 *
 * @param id mintpresso point id.
 * @param _type mintpresso point's type 
 * @param identifier mintpresso  point's identifier. identifier should be unique
 * @param data mintpresso mintpresso point's additional data. json string
 * @param _url mintpresso mintpresso point's url
 *
 */
case class Point(id: Long, _type: String, identifier: String,
                 data: String, _url: String)

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
  /** Add a point to mintpresso
   *
   * {{{
   * scala> affogato.set("user", "admire9@gmail.com")
   * Boolean = true
   *
   * }}} 
   *
   * @param _type type of point
   * @param identifier identifier of point
   * @param data additional data of point
   * @return a value whether request is successfully ended or not
   *
   */
  def set(_type: String, identifier: String, data: String = "{}"): Option[Point] = {
    val parsedData = parse(data)
    if(parsedData == JNothing) throw new AffogatoInvalidJsonException("A point data is invalid. data: String = %s".format(data))

    val point = 
      ("point" -> 
        ("type" -> _type) ~
        ("identifier" -> identifier) ~
        ("data" -> parsedData))

    val additionalData: String = compact(render(point))
    val postPointURI = uri("/account/%d/point".format(accountId))
    var req = url(postPointURI).POST

    req.addQueryParameter("api_token", token)
    req << additionalData
    req.addHeader("Content-Type", "application/json")

    Http(req OK as.String).option().map { res =>
      val json = parse(res)
      var r = for { 
        JObject(point) <- json \ "point"
        JField("id", JInt(i)) <- point 
        JField("type", JString(t)) <- point 
        JField("identifier", JString(iden)) <- point 
        JField("data", JObject(data)) <- point
        JField("_url", JString(u)) <- point
      } yield Point(i.toLong, t, iden, compact(render(data)), u)

      Some(r.head)
    }.getOrElse {
      None
    }
  }

  /** Get a edge from mintpresso
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
  def get(subjectType: String, subjectId: String, verb: String,
          objectType: String, objectId: String ): Boolean = {
    true
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
    val protocol = "http"
    val url = "mintpresso.com"
    val port: Long = 9001 
    val versionPrefix = "v1"

    "%1$s://%2$s:%3$d/%4$s%5$s".format(protocol, url, port,
                                       versionPrefix, path)
  }

  override def toString(): String = "Affogato(%1$s, %2$s)".format(token , accountId)
}


/** Invalid Json Exception Class
 *
 */
class AffogatoInvalidJsonException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
    def this() = this("", null)
     
    def this(message: String) = this(message, null)
     
    def this(nestedException : Throwable) = this("", nestedException)
}