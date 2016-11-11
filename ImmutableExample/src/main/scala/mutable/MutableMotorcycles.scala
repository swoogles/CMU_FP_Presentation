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
      throw new Exception("Car and driver aren't in the same place!") // Abort!
    }
    this.location = destination
    motorcycle.drive(destination)
  }

  def fill(motorcycle: Motorcycle): Unit = {
    if(this.location != motorcycle.location) {
      throw new Exception("Car and driver aren't in the same place!") // Abort!
    }
    motorcycle.fuel = 100
  }
}
