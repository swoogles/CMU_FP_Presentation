package com.bfrasure.fp.cmu.mutable

import org.scalatest.FlatSpec

class MutableCarTests extends FlatSpec {

  var sam = Person("Sam", Home)
  var joe = Person("Joe", Home)
  var car = Car(100, Home)

  "Driving" should "not be possible if you aren't with the car" in {
    assert(1==1)
  }

  "Driving" should "fail if both people want to travel to different places." in {
    assert(1==1)
  }

  "Driving" should "cease when you run out of gas" in {
    assertThrows[Exception] {
      if (car.fuel < 100)
        joe.fill(car)

      if (car.fuel == 100) {
        sam.drive(car, Restaurant)
        sam.drive(car, School)
        sam.drive(car, Home)
        sam.drive(car, School)
        sam.drive(car, Home)
        joe.drive(car, School) // Error: Out of gas!
        println("made it to the end?")
      }
    }
  }

//  def scenes = {
//    Scenarios.processScenes(
//      SAM, JOE, CAR,
//      Intentions(School, Home)
//    )
//
//    if (CAR.location == Home) { // Now we're safe. Sort've.
//      println("Sam's driving")
//      Person.drive(SAM, CAR, School)
//    }
//
//    Person.drive(JOE, CAR, Home)
//
//    if (CAR.condition != Pristine)
//      Person.clean(JOE, CAR)
//
//    Person.drive(JOE, CAR, JanesHouse)
//    Person.drive(JOE, CAR, Home)
//
//    if (CAR.fuel < 100)
//      Person.fill(JOE, CAR)
//  }

}
