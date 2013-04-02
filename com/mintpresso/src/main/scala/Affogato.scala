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
 *
 * @constructor Create a new affogato with api key
 * @param key mintpresso api key
 * @return A instance of Affogato
 */
class Affogato(key: String) {
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
}