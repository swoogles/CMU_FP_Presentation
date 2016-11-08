package mutable

sealed trait Location
object School extends Location
object Home extends Location
object Restaurant extends Location

case class Car(var fuel: Int, var location: Location) {
  val tripCost = 20
  def drive(destination: Location): Unit = {
    if (this.fuel < tripCost)
      throw new Exception("Out of gas!") // Abort!
    this.fuel -= tripCost
    this.location = destination
  }
}

case class Person(var name: String, var location: Location) {
  def drive(car: Car, destination: Location): Unit = {
    if(this.location == car.location) {
      throw new Exception(s"${this.name} is at ${this.location}, but Car is at ${car.location}") // Abort!
    }
    // Which of these 2 should go first?
    this.location = destination // Intentional danger here. If car.drive fails, then somehow the passenger managed to
                                // travel without their vehicle
    println("changed driver's location to : " + this.location)
    car.drive(destination)
  }

  def fill(car: Car): Unit = {
    if(this.location == car.location) {
      throw new Exception(s"${this.name} is at ${this.location}, but Car is at ${car.location}") // Abort!
    }
    car.fuel = 100
  }
}

object MutableScenarios {
  var sam: Person = null
  var joe: Person = null
  var mutableCar: Car = null

  def resetVars(): Unit = {
    sam = Person("Sam", Home)
    joe = Person("Joe", Home)
    mutableCar = Car(100, Home)
  }
}
