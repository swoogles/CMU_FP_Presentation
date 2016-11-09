package immutable

import scala.util.{Failure, Success, Try}

sealed trait Location
case object School extends Location
case object Home extends Location
case object Restaurant extends Location

case class Car(fuel: Int, location: Location)
object Car {
  val tripCost = 20

  def drive(car: Car, destination: Location): Try[Car] = {
    car match {
      case Car(_, location) if (location == destination) => Success(car)
      case Car(fuel, _) if (fuel >= tripCost) => Success(Car(car.fuel - tripCost, destination))
      case _ => Failure(new Exception("Ran out of gas!"))
    }
  }
}

case class Person(name: String, location: Location)
object Person {
  def drive(person: Person, car: Car, destination: Location): Try[(Person, Car)] = {
    person.location match {
      case `destination` => Success((person, car)) // TODO See if this backtick crap can be avoided.
      case car.location =>
        Car.drive(car, destination) flatMap { movedCar =>
          Success((person.copy(location = destination), movedCar))
        }
      case invalidLocation => Failure(new Exception( "Car and driver aren't in the same place!"))
    }
  }

  // TODO: Make person parameter relevant in some way.
  //       Possibly just accept that it doesn't have a software need here.
  def fill(person: Person, car: Car): Try[Car] =
    person.location match {
      case car.location => Success(car.copy(fuel = 100))
      case _ => Failure(new Exception( s"${person.name} is at ${person.location}, but Car is at $car.location" ))
    }
}

case class OccupiedCar(passenger: Person, car: Car) {
  def drive(destination: Location): Try[OccupiedCar] = {
    destination match {
      case passenger.location => Success(this)
      case newDestination =>
        Car.drive(car, destination) flatMap { movedCar =>
          Success(OccupiedCar(passenger.copy(location = destination), movedCar))
        }
    }
  }

  def driveNoTry(destination: Location): OccupiedCar = {
    destination match {
      case passenger.location => this
      case newDestination =>
        val driveResult: Try[OccupiedCar] = Car.drive(car, destination) map { movedCar =>
          OccupiedCar(passenger.copy(location = destination), movedCar)
        }
        driveResult.getOrElse(this)
    }
  }

}

case class Intentions(joe: Location, sam: Location)
case class Intention(person: Person, destination: Location)

object Scenarios {

  def updateScene(joe: Person, sam: Person, car: Car, intentions: Intentions): Try[(Person, Person, Car)] = {
    for ((newJoe, newCar) <- Person.drive(joe, car, intentions.joe);
         (newSam, finalCar) <- Person.drive(sam, newCar, intentions.sam) ) yield {
      (newJoe, newSam, finalCar)
    }
  }
  // TODO consider a version which returns the last successful state of things.

  def processScenes(joe: Person, sam: Person, car: Car, intentions: Intentions*): Try[(Person, Person, Car)] =
    processScenes(joe, sam, car, intentions.toList)

  def processScenes(joe: Person, sam: Person, car: Car, intentions: List[Intentions]): Try[(Person, Person, Car)] = {
    intentions.foldLeft(Try((joe, sam, car))) {
      case (Success((curJoe, curSam, curCar)), curIntentions) => updateScene(curJoe, curSam, curCar, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }

  def processScenesCumulative(joe: Person, sam: Person, car: Car, intentions: Intentions*): List[Try[(Person, Person, Car)]] =
    processScenesCumulative(joe, sam, car, intentions.toList)

  def processScenesCumulative(joe: Person, sam: Person, car: Car, intentions: List[Intentions]): List[Try[(Person, Person, Car)]] = {
    intentions.scanLeft(Try((joe, sam, car))) {
      case (Success((curJoe, curSam, curCar)), curIntentions) => updateScene(curJoe, curSam, curCar, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }


}