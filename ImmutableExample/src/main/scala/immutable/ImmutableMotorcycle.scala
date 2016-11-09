package immutable

import scala.util.{Failure, Success, Try}

sealed trait Location
case object School extends Location
case object Home extends Location
case object Restaurant extends Location

case class Motorcycle(fuel: Int, location: Location)
object Motorcycle {
  val tripCost = 20

  def drive(motorcycle: Motorcycle, destination: Location): Try[Motorcycle] = {
    motorcycle match {
      case Motorcycle(_, location) if (location == destination) => Success(motorcycle)
      case Motorcycle(fuel, _) if (fuel >= tripCost) => Success(Motorcycle(motorcycle.fuel - tripCost, destination))
      case _ => Failure(new Exception("Ran out of gas!"))
    }
  }
}

case class Person(name: String, location: Location)
object Person {
  def drive(person: Person, motorcycle: Motorcycle, destination: Location): Try[(Person, Motorcycle)] = {
    person.location match {
      case `destination` => Success((person, motorcycle)) // TODO See if this backtick crap can be avoided.
      case motorcycle.location =>
        Motorcycle.drive(motorcycle, destination) flatMap { movedCar =>
          Success((person.copy(location = destination), movedCar))
        }
      case invalidLocation => Failure(new Exception( "Car and driver aren't in the same place!"))
    }
  }

  // TODO: Make person parameter relevant in some way.
  //       Possibly just accept that it doesn't have a software need here.
  def fill(person: Person, motorcycle: Motorcycle): Try[Motorcycle] =
    person.location match {
      case motorcycle.location => Success(motorcycle.copy(fuel = 100))
      case _ => Failure(new Exception( s"${person.name} is at ${person.location}, but Car is at $motorcycle.location" ))
    }
}

case class OccupiedMotorcycle(passenger: Person, car: Motorcycle) {
  def drive(destination: Location): Try[OccupiedMotorcycle] = {
    destination match {
      case passenger.location => Success(this)
      case newDestination =>
        Motorcycle.drive(car, destination) flatMap { movedCar =>
          Success(OccupiedMotorcycle(passenger.copy(location = destination), movedCar))
        }
    }
  }

  def driveNoTry(destination: Location): OccupiedMotorcycle = {
    destination match {
      case passenger.location => this
      case newDestination =>
        val driveResult: Try[OccupiedMotorcycle] = Motorcycle.drive(car, destination) map { movedCar =>
          OccupiedMotorcycle(passenger.copy(location = destination), movedCar)
        }
        driveResult.getOrElse(this)
    }
  }

}

case class Intentions(joe: Location, sam: Location)
case class Intention(person: Person, destination: Location)

object Scenarios {

  def updateScene(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Intentions): Try[(Person, Person, Motorcycle)] = {
    for ((newJoe, newCar) <- Person.drive(joe, motorcycle, intentions.joe);
         (newSam, finalCar) <- Person.drive(sam, newCar, intentions.sam) ) yield {
      (newJoe, newSam, finalCar)
    }
  }
  // TODO consider a version which returns the last successful state of things.

  def processScenes(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Intentions*): Try[(Person, Person, Motorcycle)] =
    processScenes(joe, sam, motorcycle, intentions.toList)

  def processScenes(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: List[Intentions]): Try[(Person, Person, Motorcycle)] = {
    intentions.foldLeft(Try((joe, sam, motorcycle))) {
      case (Success((curJoe, curSam, curMotorcycle)), curIntentions) => updateScene(curJoe, curSam, curMotorcycle, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }

  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Intentions*): Either[(Throwable, (Person, Person, Motorcycle)),(Person, Person, Motorcycle)] =
    processScenesKeepLastGoodState(joe, sam, motorcycle, intentions.toList)

  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: List[Intentions]): Either[(Throwable, (Person, Person, Motorcycle)),(Person, Person, Motorcycle)] = {
    val startState: Either[(Throwable, (Person, Person, Motorcycle)),(Person, Person, Motorcycle)] = Right((joe, sam, motorcycle))
    intentions.foldLeft(startState) {
      case (Right((curJoe, curSam, curMotorcycle)), curIntentions) => {
        updateScene(curJoe, curSam, curMotorcycle, curIntentions) match {
          case Success(sceneTuple) => Right(sceneTuple)
          case Failure(ex) => Left((ex, (curJoe, curSam, curMotorcycle)))
        }
      }
      case (Left(lastGoodStateWithException), curIntentions) => Left(lastGoodStateWithException)
    }
  }

  def processScenesCumulative(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Intentions*): List[Try[(Person, Person, Motorcycle)]] =
    processScenesCumulative(joe, sam, motorcycle, intentions.toList)

  def processScenesCumulative(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: List[Intentions]): List[Try[(Person, Person, Motorcycle)]] = {
    intentions.scanLeft(Try((joe, sam, motorcycle))) {
      case (Success((curJoe, curSam, curMotorcycle)), curIntentions) => updateScene(curJoe, curSam, curMotorcycle, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }


}