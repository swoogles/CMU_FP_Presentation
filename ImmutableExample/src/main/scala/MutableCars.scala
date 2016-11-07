package com.bfrasure.fp.cmu.mutable

sealed trait Condition
object Pristine extends Condition
object Decent extends Condition
object Trashed extends Condition

sealed trait Location
object School extends Location
object Home extends Location
object Restaurant extends Location
object JanesHouse extends Location

case class Car(var fuel: Int, var condition: Condition, var location: Location) {
  val tripCost = 20
  def drive(destination: Location): Unit = {
    if (this.fuel <= 0)
      throw new Exception("Out of gas!") // Abort!
    this.fuel -= tripCost
    this.location = destination
  }
}
case class Person(var name: String, var location: Location) {
  def assertWithException(condition: Boolean, msg: String): Unit = {
    if (!condition)
      throw new Exception(msg)
  }

  def drive(car: Car, destination: Location): Unit = {
    assertWithException(this.location == car.location, s"${this.name} is at ${this.location}, but Car is at $car.location") // Abort!
    car.drive(destination)
    this.location = destination
  }
  def clean(car: Car): Unit =
    car.condition = Pristine

  def fill(car: Car): Unit =
    car.fuel = 100
}

  object MutableScenarios {
    var sam: Person = null
    var joe: Person = null
    var mutableCar: Car = null

    def resetVars(): Unit = {
      sam = Person("Sam", Home)
      joe = Person("Joe", Home)
      mutableCar = Car(100, Pristine, Home)
    }
  }
