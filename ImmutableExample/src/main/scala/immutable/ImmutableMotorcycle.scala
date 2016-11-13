package immutable

import functionalpresentation.Location

import scala.util.{Failure, Success, Try}

case class Motorcycle(fuel: Int, location: Location)
case class Person(name: String, location: Location)
// Figure out how to accomodate filling the car in here.
// Otherwise it's not a true alternative to the OO approach.
case class SceneUpdate(joe: Location, sam: Location)

case class Scene(joe: Person, sam: Person, motorcycle: Motorcycle)

object TravelFunctions {
  val tripCost = 20

  def drive(motorcycle: Motorcycle, destination: Location): Try[Motorcycle] = {
    motorcycle match {
      case Motorcycle(_, location) if (location == destination) => Success(motorcycle)
      case Motorcycle(fuel, _) if (fuel >= tripCost) => Success(Motorcycle(motorcycle.fuel - tripCost, destination))
      case _ => Failure(new Exception("Ran out of gas!"))
    }
  }

  def drive(person: Person, motorcycle: Motorcycle, destination: Location): Try[(Person, Motorcycle)] = {
    person.location match {
      case `destination` => Success((person, motorcycle)) // Try to ignore the backticks
      case motorcycle.location =>
        TravelFunctions.drive(motorcycle, destination) flatMap { movedCar =>
          Success((person.copy(location = destination), movedCar))
        }
      case invalidLocation => Failure(new Exception("Car and driver aren't in the same place!"))
    }
  }

  def fill(person: Person, motorcycle: Motorcycle): Try[Motorcycle] =
    person.location match {
      case motorcycle.location => Success(motorcycle.copy(fuel = 100))
      case _ => Failure(new Exception("Car and driver aren't in the same place!"))
    }
}

object Scenarios {

  def updateScene(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: SceneUpdate)
  : Try[(Person, Person, Motorcycle)] = {
    for ((newJoe, newCar) <- TravelFunctions.drive(joe, motorcycle, intentions.joe);
         (newSam, finalCar) <- TravelFunctions.drive(sam, newCar, intentions.sam) ) yield {
      (newJoe, newSam, finalCar)
    }
  }

  private def coherentResult(vehicle1: Motorcycle, vehicle2: Motorcycle, joe: Person, sam: Person): Try[Scene] =
    vehicle1 match {
      case `vehicle2` => Success(Scene(joe, sam, vehicle1))
      case conflictingCar => Failure(new Exception("Final car positions don't agree!"))
    }

  def updateSceneTyped(scene: Scene, intentions: SceneUpdate)
  : Try[Scene] = {
    for ((newJoe, newCar) <- TravelFunctions.drive(scene.joe, scene.motorcycle, intentions.joe);
         (newSam, finalCar) <- TravelFunctions.drive(scene.sam, newCar, intentions.sam);
         coherentScene <-
           if (scene.motorcycle != newCar)// Joe moved, so that means Sam better have moved to the same place.
             coherentResult(newCar, finalCar, newJoe, newSam)
           else
             Success(Scene(newJoe, newSam, finalCar))
    ) yield {
      coherentScene
    }
  }

  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: SceneUpdate*)
  : Either[(Throwable, (Person, Person, Motorcycle)),(Person, Person, Motorcycle)] =
    processScenesKeepLastGoodState(joe, sam, motorcycle, intentions.toList)

  def processScenesKeepLastGoodState(joe: Person, sam: Person, motorcycle: Motorcycle, intentions: List[SceneUpdate])
  : Either[(Throwable, (Person, Person, Motorcycle)),(Person, Person, Motorcycle)] = {
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