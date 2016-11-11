package mutable

import org.scalatest.{Assertion, FlatSpec}
import functionalpresentation.Location
import functionalpresentation.Home
import functionalpresentation.Restaurant
import functionalpresentation.School

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

}
