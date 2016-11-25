package billding.fp.mutable

import billding.fp.{Home, Restaurant, School}
import org.scalatest.{Assertion, FlatSpec}

class MutableCarTestsWithDebugging extends FlatSpec {

  var sam = Person("Sam", Home)
  var joe = Person("Joe", Home)
  var car = Car(100, Home)

  "Driving" should "cease when you run out of gas" in {
      if (car.fuel < 100)
        joe.fill(car)

      if (car.fuel == 100) {
        println("1")
        println("car.fuel: " + car.fuel)
        sam.drive(car, Restaurant)
        println("2")
        println("car.fuel: " + car.fuel)
        sam.drive(car, School)
        println("3")
        println("car.fuel: " + car.fuel)
        sam.drive(car, Restaurant)
        println("3")
        println("car.fuel: " + car.fuel)
        sam.drive(car, Home) // Error: Out of gas!
        println("4")
        println("car.fuel: " + car.fuel)
        sam.clean(car)
        println("Car's clean. We've got 20 units of fuel left. Let's drive!")
        joe.drive(car, Home) // Error: Out of gas!
        println("Driving completed successfully!")
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
