package mutable

sealed trait Location
object School extends Location
object Home extends Location
object Restaurant extends Location

case class Motorcycle(var fuel: Int, var location: Location) {
  val tripCost = 20
  def drive(destination: Location): Unit = {
    if (this.fuel < tripCost)
      throw new Exception("Out of gas!") // Abort!
    this.fuel -= tripCost
    this.location = destination
  }
}

case class Person(var name: String, var location: Location) {

  // Spot the bug in this method!
  def drive(motorcycle: Motorcycle, destination: Location): Unit = {
    if(this.location != motorcycle.location) {
      throw new Exception(s"${this.name} is at ${this.location}, but Car is at ${motorcycle.location}") // Abort!
    }
    this.location = destination
    motorcycle.drive(destination)
  }

  def fill(motorcycle: Motorcycle): Unit = {
    if(this.location != motorcycle.location) {
      throw new Exception(s"${this.name} is at ${this.location}, but Car is at ${motorcycle.location}") // Abort!
    }
    motorcycle.fuel = 100
  }
}

object MutableScenarios {
  var sam: Person = null
  var joe: Person = null
  var motorcycle: Motorcycle = null

  def resetVars(): Unit = {
    sam = Person("Sam", Home)
    joe = Person("Joe", Home)
    motorcycle = Motorcycle(100, Home)
  }
}


