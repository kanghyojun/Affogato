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
  def set(_type: String, identifier: String, data: String = ""): Boolean = {
    true
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