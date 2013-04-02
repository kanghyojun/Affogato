package com.mintpresso.spec

import org.specs2.mutable._
import com.mintpresso._

class MintpressoSpec extends Specification {
  
  "Mintpresso API Pack" should {
    
    "Add point" in {
      Mintpresso.set("user", identifier="admire9@gmail.com", data="") === true
    }

    "Get Point" in {
      Mintpresso.get(
        "user",
        "admire9@gmail.com",
        "listen",
        "music",
        "bugs-1"
      ) === true
    }
  }
}
