package com.bfrasure.fp.cmu

import scala.util.{Failure, Success, Try}

sealed trait Condition
case object Pristine extends Condition
case object Decent extends Condition
case object Trashed extends Condition

sealed trait Location
case object School extends Location
case object Home extends Location
case object Restaurant extends Location
case object JanesHouse extends Location

case class Car(fuel: Int, condition: Condition, location: Location)
object Car {
  val tripCost = 20

  def drive(car: Car, destination: Location): Try[Car] = {
    if (car.fuel > tripCost) {
      Success(Car(car.fuel - tripCost, car.condition, destination))
    } else {
      Failure(new Exception("Ran out of gas!"))
    }

  }
}
case class Person(name: String, location: Location)
object Person {
  def drive(person: Person, car: Car, destination: Location): Try[(Person, Car)] = {
    if (person.location == destination) {
      Success((person, car))
    } else {
      if (person.location == car.location) {
        Car.drive(car, destination) flatMap { (movedCar: Car) =>
          val newPerson = Person(person.name, destination)
          Success((newPerson, movedCar))
        }
      } else {
        Failure(new Exception( "Car and driver aren't in the same place!"))
      }
    }
  }

  // TODO: Make person parameter relevant in some way.
  //       Possibly just accept that it doesn't have a software need here.
  def clean(person: Person, car: Car): Car =
  car.copy(condition = Pristine)
  def fill(person: Person, car: Car): Car =
    car.copy(fuel = 100)
}

case class Intentions(joe: Location, sam: Location)
case class IntentionsAlt(joe: Person, sam: Person) // Rather than giving destinations, you can request a precise end state.

object Scenarios {
  def updateScene(joe: Person, sam: Person, car: Car, intentions: Intentions): Try[(Person, Person, Car)] = {
    // driveTry(person: Person, car: Car, destination: Location): Try[(Person, Car)] = {
    for ((newJoe, newCar) <- Person.drive(joe, car, intentions.joe);
         (newSam, finalCar) <- Person.drive(sam, newCar, intentions.sam) ) yield {
      (newJoe, newSam, finalCar)
    }
  }

  def processScenes(joe: Person, sam: Person, car: Car, intentions: Intentions*): Try[(Person, Person, Car)] =
    processScenes(joe, sam, car, intentions.toList)

  def processScenes(joe: Person, sam: Person, car: Car, intentions: List[Intentions]): Try[(Person, Person, Car)] = {
    intentions.foldLeft(Try((joe, sam, car))) {
      case (Success((curJoe, curSam, curCar)), curIntentions) => updateScene(curJoe, curSam, curCar, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }



}