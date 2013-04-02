package com.mintpresso.spec

import org.specs2.mutable._
import com.mintpresso._

class AffogatoSpec extends Specification {
  
  val apiKey = "cc64f8ee51c8420172a907baa81285ae"
  
  "Mintpresso API Pack" should {
    
    "Add point" in {

      val affogato = new Affogato(apiKey)

      affogato.set("user", identifier="admire9@gmail.com", data="") === true
    }

    "Get Point" in {
      val affogato = new Affogato(apiKey)
      affogato.get(
        "user",
        "admire9@gmail.com",
        "listen",
        "music",
        "bugs-1"
      ) === true
    }
  }
}
