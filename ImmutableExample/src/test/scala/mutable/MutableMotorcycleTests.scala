package mutable

import org.scalatest.{Assertion, FlatSpec}

class MutableMotorcycleTests extends FlatSpec {

  var sam = Person("Sam", Home)
  var joe = Person("Joe", Home)
  var car = Motorcycle(100, Home)


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
      }
    }
  }

  "Car and driver" should "end in different locations to demonstrate subtle bug" in {
    val blah: Assertion = assertThrows[Exception] {
      car.fuel = 0
      joe.drive(car, School) // Error: Out of gas!
    }
    assert(joe.location == School)
    assert(car.location == Home)
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
