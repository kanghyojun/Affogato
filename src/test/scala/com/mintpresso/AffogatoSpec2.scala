package com.mintpresso.spec


import scala.collection.mutable.LinkedHashMap
import scala.reflect.runtime.universe._

import org.specs2.mutable._

class AffogatoConversionException(message: String, nestedException: Throwable) extends Exception(message, nestedException) {
    def this() = this("", null)
     
    def this(message: String) = this(message, null)
     
    def this(nestedException : Throwable) = this("", nestedException)
}

case class AffogatoError(messages: Seq[String])

object ResultSet {
  def apply[T: scala.reflect.runtime.universe.TypeTag](result: T): ResultSet = {
    return new ResultSet(result, typeOf[T])
  }
}
class ResultSet(result: Any, ev: scala.reflect.runtime.universe.Type) {
  import scala.reflect.runtime.universe.{TypeTag, typeOf}

  def as[A: TypeTag] = {
    if(ev =:= typeOf[A]) {
      result.asInstanceOf[A]
    } else {
      val targ = ev match { case TypeRef(_, _, args) => args }
      val msg = "result of ResultSet cannot be cast $targ"
      throw new AffogatoConversionException(msg)
    }
  }

  def fold[R, T: TypeTag](e: AffogatoError => R, s: T => R): R = {
    if(typeOf[AffogatoError] =:= ev) {
      e(this.as[AffogatoError])
    } else if(typeOf[T] =:= ev) {
      s(this.as[T])
    } else {
      val targ = ev match { case TypeRef(_, _, args) => args }
      val msg = "result of ResultSet cannot be cast $targ"
      throw new AffogatoConversionException(msg)
    }
  }
}

class DummyAffogato {
  def set(a: Long): ResultSet = {
    ResultSet(a)
  }

  def set(a: Double): ResultSet = {
    ResultSet(a)
  }
}
class AffogatoSpec2 extends Specification {
  
  "ResultSet " should {
    "type conversions" in { 
      val r = ResultSet(1L)

      r.fold(
        error => false === true,
        (longData: Long) => longData === 1L
      )

      var dummy = new DummyAffogato()

      val res = dummy.set(1L)
      val r2 = dummy.set(3.14)

      res.as[Long] === 1L
      r2.as[Double] === 3.14
    }

    "error conversion" in {
      val errMsg = "Something Goes Wrong :- ("
      val r = ResultSet(new AffogatoError(Seq[String](errMsg)))

      r.fold(
        error => {
          error.messages.head === errMsg
        },
        (wrong: Long) => false === true
      )
    }
  }
}
