package immutable

import functionalpresentation.Location

import scala.util.{Failure, Success, Try}

case class Car(fuel: Int, location: Location)
case class Person(name: String, location: Location)
// Figure out how to accomodate filling the car in here.
// Otherwise it's not a true alternative to the OO approach.
case class SceneUpdate(joe: Location, sam: Location)

case class Scene(joe: Person, sam: Person, car: Car)

object TravelFunctions {
  val tripCost = 20

  private def drive(car: Car, destination: Location): Try[Car] = {
    car match {
      case Car(_, location) if (location == destination) => Success(car)
      case Car(fuel, _) if (fuel >= tripCost) => Success(Car(car.fuel - tripCost, destination))
      case _ => Failure(new Exception("Ran out of gas!"))
    }
  }

  def drive(person: Person, car: Car, destination: Location): Try[(Person, Car)] = {
    person.location match {
      case `destination` => Success((person, car)) // Try to ignore the backticks
      case car.location =>
        TravelFunctions.drive(car, destination) flatMap { movedCar =>
          Success((person.copy(location = destination), movedCar))
        }
      case invalidLocation => Failure(new Exception("Car and driver aren't in the same place!"))
    }
  }

  def fill(person: Person, car: Car): Try[Car] =
    person.location match {
      case car.location => Success(car.copy(fuel = 100))
      case _ => Failure(new Exception("Car and driver aren't in the same place!"))
    }
}

object Scenarios {

  def updateScene(joe: Person, sam: Person, car: Car, intentions: SceneUpdate)
  : Try[(Person, Person, Car)] = {
    for ((newJoe, newCar) <- TravelFunctions.drive(joe, car, intentions.joe);
         (newSam, finalCar) <- TravelFunctions.drive(sam, newCar, intentions.sam) ) yield {
      (newJoe, newSam, finalCar)
    }
  }

  private def coherentResult(vehicle1: Car, vehicle2: Car, joe: Person, sam: Person): Try[Scene] =
    vehicle1 match {
      case `vehicle2` => Success(Scene(joe, sam, vehicle1))
      case conflictingCar => Failure(new Exception("Final car positions don't agree!"))
    }

  def updateSceneTyped(scene: Scene, intentions: SceneUpdate)
  : Try[Scene] = {
    for ((newJoe, joeCarResult) <- TravelFunctions.drive(scene.joe, scene.car, intentions.joe);
         (newSam, samCarResult) <- TravelFunctions.drive(scene.sam, scene.car, intentions.sam);
         coherentScene <-
           if (joeCarResult == samCarResult)// Joe moved, so that means Sam better have moved to the same place.
             Success(Scene(newJoe, newSam, samCarResult))
           else
             Failure(new Exception("Final car positions don't agree!"))

    ) yield {
      coherentScene
    }
  }

  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Car, intentions: SceneUpdate*)
  : Either[(Throwable, (Person, Person, Car)),(Person, Person, Car)] =
    processScenesKeepLastGoodState(joe, sam, motorcycle, intentions.toList)

  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Car, intentions: List[SceneUpdate])
  : Either[(Throwable, (Person, Person, Car)),(Person, Person, Car)] = {
    val startState: Either[(Throwable, (Person, Person, Car)),(Person, Person, Car)] = Right((joe, sam, motorcycle))
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




  // TODO Find more use cases for this. I'm glad I managed to extract the duplicate code from processScenesType and
  // processScenesTypedCumulative, but it'd really start to shine if I can get 1 or 2 more useful variations.
  private val sceneUpdateCases = { (curScene: Try[Scene], update: SceneUpdate) =>
      (curScene, update) match {
      case (Success(curScene: Scene), curIntentions) => updateSceneTyped(curScene, curIntentions)
      case (Failure(ex), curIntentions) => Failure(ex)
    }
  }

  def processScenesTyped(scene: Scene, intentions: SceneUpdate*) : Try[Scene] =
    processScenesTyped(scene, intentions.toList)


  def processScenesTyped(scene: Scene, intentions: List[SceneUpdate]) : Try[Scene] =
    intentions.foldLeft(Try(scene))(sceneUpdateCases)

  def processScenesCumulativeTyped(scene: Scene, intentions: SceneUpdate*) : List[Try[Scene]] =
    processScenesCumulativeTyped(scene, intentions.toList)

  def processScenesCumulativeTyped(scene: Scene, intentions: List[SceneUpdate]) : List[Try[Scene]] =
    intentions.scanLeft(Try(scene))(sceneUpdateCases)

  case class SceneWithFailedMoves(scene: Scene, failedMoves: List[(Scene, SceneUpdate, Throwable)])

  def processScenesFaultTolerantTyped(scene: Scene, intentions: SceneUpdate*) : SceneWithFailedMoves =
    processScenesFaultTolerantTyped(scene, intentions.toList)


  def processScenesFaultTolerantTyped(scene: Scene, intentions: List[SceneUpdate]) : SceneWithFailedMoves = {
    val sceneWithReversedFailedMoves = intentions.foldLeft(SceneWithFailedMoves(scene, List())) {
      case (curScene, curIntentions) =>
        val sceneUpdateAttempt: Try[Scene] = updateSceneTyped(curScene.scene, curIntentions)
        sceneUpdateAttempt match {
          case Success(newScene) => curScene.copy(scene = newScene)
          case Failure(ex) => curScene.copy(
            failedMoves = (curScene.scene, curIntentions, ex) :: curScene.failedMoves
          )
        }
    }
    sceneWithReversedFailedMoves.copy(failedMoves = sceneWithReversedFailedMoves.failedMoves.reverse)

  }


  def processScenesKeepLastGoodState(scene: Scene, intentions: List[SceneUpdate]) : Either[(Throwable, Scene), Scene] = {
    val startState: Either[(Throwable, Scene),Scene] = Right(scene)
    intentions.foldLeft(startState) {
      case (Right(curScene), curIntentions) => {
        updateSceneTyped(curScene, curIntentions) match {
          case Success(sceneTuple) => Right(sceneTuple)
          case Failure(ex) => Left((ex, curScene))
        }
      }
      case (Left(lastGoodStateWithException), curIntentions) => Left(lastGoodStateWithException)
    }
  }

}