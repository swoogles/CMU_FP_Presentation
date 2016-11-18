package mutable

import functionalpresentation.{Home, Restaurant, School}
import org.scalatest.{Assertion, FlatSpec}

class MutableCarTests extends FlatSpec {

  var sam = Person("Sam", Home)
  var joe = Person("Joe", Home)
  var car = Car(100, Home)

  "Driving" should "cease when you run out of gas" in {
      if (car.fuel < 100)
        joe.fill(car)

      if (car.fuel == 100) {
        sam.drive(car, Restaurant)
        sam.drive(car, School)
        sam.drive(car, Restaurant)
        sam.drive(car, Home)
        sam.clean(car) // Sam feels bad about getting the car dirty with all the driving, so he cleans it out.
        joe.drive(car, Home)
      }
  }

  "Car and driver" should "end in different locations to demonstrate subtle bug" in {
    val startLocation = joe.location
    val expectedFailure: Assertion = assertThrows[Exception] {
      car.fuel = 0
      joe.drive(car, School) // Error: Out of gas!
    }
    assert(joe.location == startLocation, "Joe should not have moved since the car was out of gas.")
  }

}
