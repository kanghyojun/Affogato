package com.mintpresso
/** Provides Scala API for mintpresso.com.
  * Affogato provides more simple and easy way to use mintpresso
  * 
  * ==Overview==
  * 
  * A Affogato class should be initialized with api key can get from mintpresso.com.
  * 
  * {{{
  * scala> val affogato = new Affogato("some-api-key-goes-here") 
  * affogato: 
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
  def set(_type: String, identifier: String, data: String): Boolean = {
    true
  }

  def get(subjectType: String, subjectId: String, verb: String,
          objectType: String, objectId: String ): Boolean = {
    true
  }
}