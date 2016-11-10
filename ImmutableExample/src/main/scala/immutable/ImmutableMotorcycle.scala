package immutable

import scala.util.{Failure, Success, Try}

sealed trait Location
case object School extends Location
case object Home extends Location
case object Restaurant extends Location

case class Motorcycle(fuel: Int, location: Location)
object MotorcycleFunctions {
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
object PersonFunctions {

  def drive(person: Person, motorcycle: Motorcycle, destination: Location): Try[(Person, Motorcycle)] = {
    person.location match {
      case `destination` => Success((person, motorcycle)) // TODO See if this backtick crap can be avoided.
      case motorcycle.location =>
        MotorcycleFunctions.drive(motorcycle, destination) flatMap { movedCar =>
          Success((person.copy(location = destination), movedCar))
        }
      case invalidLocation => Failure(new Exception( "Car and driver aren't in the same place!"))
    }
  }

  def fill(person: Person, motorcycle: Motorcycle): Try[Motorcycle] =
    person.location match {
      case motorcycle.location => Success(motorcycle.copy(fuel = 100))
      case _ => Failure(new Exception( s"${person.name} is at ${person.location}, but Car is at $motorcycle.location" ))
    }
}

case class Movements(joe: Location, sam: Location)

object Scenarios {

  /**
    * Attempt to apply movements to the scene in its current state.
    */
  def updateScene(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Movements): Try[(Person, Person, Motorcycle)] = {
    for ((newJoe, newCar) <- PersonFunctions.drive(joe, motorcycle, intentions.joe);
         (newSam, finalCar) <- PersonFunctions.drive(sam, newCar, intentions.sam) ) yield {
      (newJoe, newSam, finalCar)
    }
  }

  def processMovements(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Movements*): Try[(Person, Person, Motorcycle)] =
    processMovements(joe, sam, motorcycle, intentions.toList)

  def processMovements(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: List[Movements]): Try[(Person, Person, Motorcycle)] = {
    intentions.foldLeft(Try((joe, sam, motorcycle))) {
      case (Success((curJoe, curSam, curMotorcycle)), curIntentions) => updateScene(curJoe, curSam, curMotorcycle, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }

  def processScenesCumulative(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Movements*): List[Try[(Person, Person, Motorcycle)]] =
    processScenesCumulative(joe, sam, motorcycle, intentions.toList)

  def processScenesCumulative(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: List[Movements]): List[Try[(Person, Person, Motorcycle)]] = {
    intentions.scanLeft(Try((joe, sam, motorcycle))) {
      case (Success((curJoe, curSam, curMotorcycle)), curIntentions) => updateScene(curJoe, curSam, curMotorcycle, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }

  // Maybe show the output from these functions, but probably don't show implementation
  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: Movements*): Either[(Throwable, (Person, Person, Motorcycle)),(Person, Person, Motorcycle)] =
    processScenesKeepLastGoodState(joe, sam, motorcycle, intentions.toList)

  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: List[Movements]): Either[(Throwable, (Person, Person, Motorcycle)),(Person, Person, Motorcycle)] = {
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

//  val someCase = (attempt: Try[(Person, Person, Motorcycle)]) => attempt match { case(Success((curJoe, curSam, curMotorcycle)), curIntentions) => "blah" }



}