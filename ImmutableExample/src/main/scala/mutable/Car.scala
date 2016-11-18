package mutable

import functionalpresentation.Location

trait Movable {
  def move(destination: Location): Unit
}

trait Driver {
  def drive(car: Car, destination: Location): Unit
  def fill(car: Car): Unit
  def clean(car: Car): Unit
}

case class Car(var fuel: Int, var location: Location) extends Movable {
  val tripCost = 20
  def move(destination: Location): Unit = {
    if (this.fuel < tripCost)
      throw new RuntimeException("Out of gas!") // Abort!
    this.fuel -= tripCost
    this.location = destination
  }
}

case class Person(var name: String, var location: Location) extends Driver {

  // Spot the bug in this method!
  def drive(car: Car, destination: Location): Unit = {
    if(this.location != car.location) {
      throw new RuntimeException("Car and driver aren't in the same place!") // Abort!
    }
    this.location = destination
    car.move(destination)
  }

  def fill(car: Car): Unit = {
    if(this.location != car.location) {
      throw new Exception("Car and driver aren't in the same place!") // Abort!
    }
    car.fuel = 100
  }

  def clean(car: Car): Unit = {
    if(this.location != car.location) {
      throw new Exception("Car and driver aren't in the same place!") // Abort!
    }
    car.fuel -= 10
  }
}
